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
  (let ((super (or (first direct-superclasses)
                   'java/lang:object)))
    (setf (gethash name (native-classes *3bil2-environment*))
          (make-instance
           'native-class
           :access '(:public :super)
           :attributes nil
           :extends super
           :implements (cdr direct-superclasses)
           :methods (collect-inherited-methods name super)
           :fields nil
           :java-name (print
                       (substitute #\/ #\:
                                   (let ((*package* (find-package :keyword)))
                                     (format nil "~(~s~)" name))))
           :name name)))
  nil)

(defmacro/3bil2 defmethod-native (name &rest args)
  ;; we do all work at macroexpansion time and return NIL since
  ;; everything this does needs to be compiled into the .dex file.
  ;; slightly messier than defclass case, since we actually need to
  ;; compile the body... need to rethink APIs a bit...
  (let ((visibility (if (listp (car args))
                        :private
                        (pop args)))
        (lambda-list (pop args))
        (body args))
    (format t "~&defmethod-native ~s ~s ~s~%" name visibility lambda-list)
    (unless (every 'consp lambda-list)
      (error "you need to specify types for all arguments to native method for now...~% (got ~s )" lambda-list))
    (let ((code (compile-toplevel-1
                 (print
                  `(function
                    (lambda ,(mapcar 'first lambda-list)
                     (declare ,@ (loop for (name type) in lambda-list
                                       collect (list 'type type name)))
                     (macrolet
                         ((call-next-method (&rest a)
                            `(,',name
                              (super ,',(first (first lambda-list)))
                              ,@(or (cdr a)
                                    ',(loop for (name nil)
                                              in (cdr lambda-list)
                                            collect name)))))
                       ,@body)))))))
      ;; todo: store the generated code somewhere...
      (format t "~&method body =>~%  ~s~%" code))
    nil))
