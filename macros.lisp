(in-package 3bil2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod cst::destructure-parameter-groups
      ((client 3bil2) (parameter-groups cl:null)
       argument-variable tail-variable body)
    `(let ((,tail-variable ,argument-variable))
       (declare (ignorable ,tail-variable)
                ;; turn sbcl compiler notes back on for user code
                #+sbcl (sb-ext:unmuffle-conditions sb-ext:compiler-note))
       ,body))

  (defmethod cst::destructure-lambda-list
      ((client 3bil2) ll
       argument-variable tail-variable body)
    ;; silence some SBCL noise about deleted code in generated code
    `(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
       ,(call-next-method))))


(defmacro defmacro/3bil2 (name lambda-list &body body)
  (let ((ex (cst:parse-macro *3bil2-system*
                             name
                             (ensure-cst lambda-list)
                             (cons 'progn body)
                             *3bil2-environment*)))

    `(progn
       (when (sicl-genv:fboundp ',name *3bil2-environment*)
         (format t "~&redefining macro ~s, was ~s~%"
                 ',name (sicl-genv:fdefinition ',name *3bil2-environment*)))
       (setf (sicl-genv:macro-function ',name *3bil2-environment*)
             (compile nil ',ex))
       ',name)))



(defmacro/3bil2 defclass-native (name direct-superclasses direct-slots
                                      &rest options)
  ;; we do all work at macroexpansion time and return NIL since
  ;; everything this does needs to be compiled into the .dex file
  (format t "~&defclass-native ~s ~s ~s ~s~%" name direct-superclasses
          direct-slots options)
  (let* ((super (or (first direct-superclasses)
                    'java/lang:object))
         (jname
           (substitute
            #\_ #\-
            (format nil "~(~a/~a~)" (package-name (symbol-package name))
                    (symbol-name name))))
         (c (make-instance
             'native-class
             :access '(:public)
             :attributes nil
             :extends super
             :implements (cdr direct-superclasses)
             :methods (collect-inherited-methods name super)
             :fields nil
             :java-name (print jname)
             :name name)))
    (setf (gethash name (native-classes *3bil2-environment*))
          c)
    (setf (slot-value c 'fields)
          (coerce
           (loop for s in direct-slots
                 for name = (car s)
                 for type = (getf (cdr s) :type)
                 do (assert type)
                 collect (make-instance 'native-slot
                                        :name name
                                        :from (java-name c)
                                        :native-class c
                                        :field-name (string-downcase name)
                                        :type (java-type-string-for-class
                                               type)
                                        :access '(:public)
                                        :attributes nil))
           'vector)))
  (add-default-constructor (gethash name (native-classes *3bil2-environment*)))
  nil)


(defmacro/3bil2 defmethod-native (name &rest args)
  ;; we do all work at macroexpansion time and return NIL since
  ;; everything this does needs to be compiled into the .dex file.
  ;; slightly messier than defclass case, since we actually need to
  ;; compile the body... need to rethink APIs a bit...
  (let ((visibility (list (if (listp (car args))
                              :private
                              (pop args))))
        (lambda-list (pop args))
        (body args)
        (this-type))
    (format t "~&defmethod-native ~s ~s ~s~%" name visibility lambda-list)
    (unless (every 'consp lambda-list)
      (error "you need to specify types for all arguments to native method for now...~% (got ~s)" lambda-list))
    (setf this-type (second (first lambda-list)))
    (when (eq name 'java/lang/object:<init>)
      (push :constructor visibility))
    (let ((code (compile-toplevel-1
                 (print
                  `(function
                    (lambda ,(mapcar 'first lambda-list)
                     (declare ,@(loop for (name type) in lambda-list
                                      collect (list 'type type name)))
                     (%asm
                      ,@(loop for (name type) in lambda-list
                              collect (list :%declare-type
                                            name type)))
                     (macrolet
                         ((call-next-method (&rest a)
                            `(,',name
                              (super ,',(first (first lambda-list)))
                              ,@(or (cdr a)
                                    ',(loop for (name nil)
                                              in (cdr lambda-list)
                                            collect name)))))
                       ,@body)))))))
      ;; todo: better structured return from compile-toplevel
      (destructuring-bind ((asm args regs sig ret outs) ir ast) code
        (declare (ignore ir ast))
        (let ((m (gethash name (native-methods *3bil2-environment*))))
          (unless m
            (setf (gethash name (native-methods *3bil2-environment*))
                  (make-instance 'native-method-function-info
                                 :field-name (string-downcase (string name))
                                 :name name
                                 :native-class this-type
                                 :from (java-name
                                        (gethash this-type
                                                 (native-classes
                                                  *3bil2-environment*)))))
            (setf m (gethash name (native-methods *3bil2-environment*))))
          (unless (gethash this-type (signatures m))
            (setf (gethash this-type (signatures m))
                  (make-hash-table :test 'equalp)))
          (pushnew m (methods
                      (gethash this-type (native-classes *3bil2-environment*))))
          (setf (gethash sig (gethash this-type (signatures m)))
                (list this-type sig visibility nil
                      (list :asm asm :args args :regs regs :ret ret
                            :outs outs))))))
    nil))

(defmacro/3bil2 new (class &rest arguments)
  (let ((nc (gethash class (native-classes *3bil2-environment*))))
    (unless nc
      (error "tried to NEW unknown class ~s?" class))
    (alexandria:with-gensyms (instance tmp)
      `(let ((,instance (%asm
                         ,tmp
                         (:new-instance ,tmp
                                        ,(java-type-string-for-class nc)))))
         (java/lang/object:<init> ,instance ,@arguments)
         (%asm (:%declare-type ,instance ,class))
         ,instance))))

(defmacro/3bil2 %set-svref (value vector index)
  (alexandria:once-only (value index)
    `(%asm
      (:aput ,value ,vector ,index))))

(defmacro/3bil2 svref (vector index)
  (alexandria:with-gensyms (result)
    (alexandria:once-only (vector index)
      `(progn
         (%asm
          ,result
          (:aget ,result ,vector ,index))
         )
      )))

(defmacro/3bil2 %vector (class &body contents)
  (let ((nc (gethash class (native-classes *3bil2-environment*))))
    (unless nc
      (error "tried to create array of unknown class ~s?" class))
    ;; for now just using :new-array and filling manually
    (alexandria:with-gensyms (vec tmp l)
      (print
       `(let* ((,l ,(length contents))
               (,vec (%asm
                      ,tmp
                      (:new-array ,tmp
                                  ,l
                                  ,(format nil "[~a"
                                           (java-type-string-for-class nc))))))
          (%asm
           (:%declare-type ,vec (vector ,class)))
          ,@(loop for c in contents
                  for i from 0
                  collect `(%set-svref ,c ,vec ,i)
                  #++collect `(:aput ,c ,i ,vec))
          ,vec)))))

(defmacro/3bil2 the (type value)
  (alexandria:once-only (value)
    `(progn
       (%asm
        (:check-cast ,value ,type))
       ,value)))

(defmacro/3bil2 when (condition &body body)
  `(if ,condition
       (progn ,@body)
       (values)))

(defmacro/3bil2 eql (a b)
 (alexandria:with-gensyms (result l)
    (print
     (alexandria:once-only (a b)
       `(%asm
         ,result
         (:const ,result 1)
         (:if-eq ,a ,b ,l)
         (:const ,result 0)
         (:label ,l)
         )))))


;;; make them accessible from CL for slime C-c C-c use
;; probably should do this automatically, but need to put them in a
;; special package so we don't overwrite things like CL:DEFUN
(defmacro defmethod-native (&whole w name lambda-list &body body)
  (declare (ignorable name lambda-list body))
  `(progn
     (format t " compiling method ~s~%" ',name)
     (compile-toplevel-1 ',w)))

(defmacro defclass-native (&whole w name superclasses &body body)
  (declare (ignorable name superclasses body))
  `(progn
     (format t " compiling class ~s~%" ',name)
     (compile-toplevel-1 ',w)))
