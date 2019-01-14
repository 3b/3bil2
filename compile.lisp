(in-package 3bil2)

(defun collect-inherited-methods (class super-class)
  (loop for m in (public-methods
                  super-class
                  :include-protected t)
        for sigs = (loop for (sc s a ?)
                           in (alexandria:hash-table-values
                               (gethash super-class (signatures m)))
                         count 1
                         ;; fixme: abstract this
                         do (unless (gethash class (signatures m))
                              (setf (gethash class (signatures m))
                                    (make-hash-table :test 'equal)))
                            (setf (gethash s
                                           (gethash class (signatures m)))
                                  (list class s a ?)))
        when (plusp sigs)
          collect m))

(defun empty-form-p (ast)
  (or (null ast)
      (and (typep ast 'cleavir-ast:load-time-value-ast)
           (or (not (cleavir-ast:form ast))
               (equalp ''nil (cleavir-ast:form ast))))))


;; copied from cleavir-remove-useless-instructions:remove-useless-instructions
;; since we need to keep some more things
(defgeneric instruction-may-be-removed-p (instruction))

(defmethod instruction-may-be-removed-p (instruction)
  (and (= (length (cleavir-ir:successors instruction)) 1)
       (loop for output in (cleavir-ir:outputs instruction)
	     always (null (cleavir-ir:using-instructions output)))))

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:side-effect-mixin))
  nil)

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:enter-instruction))
  nil)

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:the-instruction))
  nil)

(defmethod instruction-may-be-removed-p
    ((inst asm-instruction))
  nil)
(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:the-values-instruction))
  nil)

(defun remove-useless-instructions (initial-instruction)
  (loop
    do (cleavir-ir:reinitialize-data initial-instruction)
       (let ((useless-instructions '()))
	 (cleavir-ir:map-instructions-arbitrary-order
	  (lambda (instruction)
	    (when (instruction-may-be-removed-p instruction)
              (format t "~&remove ~s~%" instruction)
	      (push instruction useless-instructions)))
	  initial-instruction)
	 (when (null useless-instructions)
	   (loop-finish))
	 (mapc #'cleavir-ir:delete-instruction useless-instructions))))


(defun compile-toplevel-1 (form &key env)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
         (sys (make-instance '3bil2))
         (env (or env *3bil2-environment*))
         (cst (cst:cst-from-expression form))
         (ast (cleavir-cst-to-ast:cst-to-ast cst env sys))
         (hir (unless (empty-form-p ast)
                (cleavir-ast-to-hir:compile-toplevel-unhoisted ast))))

    (when hir
      (cleavir-ast-graphviz:draw-ast ast "/tmp/ast.dot")
      (cleavir-ir-graphviz:draw-flowchart hir "/tmp/hir.dot")
      (cleavir-partial-inlining:do-inlining hir)
      (cleavir-ir-graphviz:draw-flowchart hir "/tmp/hir1.dot")
      (cleavir-kildall-type-inference:infer-types hir env :prune t)
      (cleavir-hir-transformations:eliminate-typeq hir)
      (cleavir-hir-transformations:eliminate-superfluous-temporaries hir)
      (remove-useless-instructions hir)
      (cleavir-ir-graphviz:draw-flowchart hir "/tmp/hir2.dot")
      (list (multiple-value-list
             (compile-hir hir env
                          (when (typep ast 'native-method-ast)
                            (native-method-arg-types ast))))
            hir ast))))

(defun compile-toplevel (form &key env)
  (if (typep form '(cons (eql progn)))
      (remove 'nil (mapcar (lambda (a) (compile-toplevel-1 a :env env))
                           (cdr form)))
      (compile-toplevel-1 form :env env)))

(defun 3bil2-compile-file (file)
  (let ((*package* *package*))
    (format t " compiling file ~s~%" file)
    (with-open-file (i file)
      (loop for f = (read i nil i)
            until (eq f i)
            when (typep f '(cons (eql in-package)
                            (cons (or symbol string) null)))
              do (setf *package* (find-package (second f)))
            else
              do (format t "~&compiling form:~% ~s~%" f)
                 (compile-toplevel-1 f)))))

(defun add-default-constructor (class)
  (let ((m (gethash 'java/lang/object:<init>
                    (native-methods *3bil2-environment*)))
        (this-type (name class))
        (sig "()V"))
    (unless (gethash this-type (signatures m))
      (setf (gethash this-type (signatures m))
            (make-hash-table :test 'equalp)))
    (pushnew m (methods class))
    (labels ((jc (x)
               (format nil "L~a;" x))
             (super ()
               (jc (java-name
                    (gethash (extends class)
                             (native-classes *3bil2-environment*))))))
      (let ((code
              `(,this-type
                "()V"
                (:constructor :public)
                nil
                (:asm
                 ((:invoke-direct
                   (,(super)
                     ("V" "V" #()) "<init>" nil) 0)
                  (:return-void))
                 :args (,(jc (java-name class)))
                 :regs 1 :ret "V"))))
        (setf (gethash sig (gethash this-type (signatures m)))
              code)))))

(defun ensure-resource-classes (package resources)
  (let* ((rpn (format nil "~a/R" package))
         (rp (or (find-package rpn) (make-package rpn :use nil)))
         (pp (or (find-package package) (make-package package :use nil)))
         (resource-classes (make-hash-table)))
    (loop for (s v) in resources
          do (let* ((sym (intern s rp))
                    (split (split-sequence:split-sequence #\/ s))
                    (.class (format nil "R$~a" (first split)))
                    (class (intern (string-upcase .class) pp))
                    (.field (second split))
                    (field (intern .field rp)))
               (export sym rp)
               (export class pp)
               (define-constant sym v)
               (push (list sym v field)
                     (gethash class resource-classes))))
    (register-ffi-class (intern "R" pp) :name (format nil "~(~a~)/R" package)
                                        :extends 'java/lang:object
                                        :access '(:public :synthetic :final)
                                        :fields #()
                                        :methods #())
    (export (intern "R" pp) pp)
    (loop for c being the hash-keys of resource-classes
            using (hash-value fields)
          for jname = (format nil "~(~a~)/R~(~a~)"
                              package
                              (subseq (string c) 1))
          for class = (register-ffi-class c :name jname
                                            :extends 'java/lang:object
                                            :access '(:public :synthetic
                                                      :final))
          do (add-default-constructor class)
             (setf (slot-value class 'fields)
                   (coerce
                    (loop for (s v f) in fields
                          collect
                          (make-instance 'native-slot
                                         :name f
                                         :from jname
                                         :native-class class
                                         :field-name (string-downcase f)
                                         :type "I"
                                         :access '(:public :synthetic
                                                   :static :final)
                                         :attributes `(:constant-value ,v)))
                    'vector)))))

(defun link-methods (class)
  (let ((methods
          (remove-duplicates
           (loop
             for m in (methods class)
             append (loop for sig in (alexandria:hash-table-values
                                      (gethash (name class) (signatures m)))
                          when (fifth sig)
                            collect (cons m sig)))
           :test 'equalp)))
    (loop for (m c sig access nil code) in methods
          for name = (field-name m)
          for dcm = (destructuring-bind (&key asm regs args ret (outs 0)) code
                      (make-instance
                       '3b-dex::dex-class-method
                       :annotations nil
                       :code (make-instance '3b-dex::dex-code
                                            :debug-info nil
                                            :instructions asm
                                            :tries nil
                                            :ins (length args)
                                            :outs (max outs (length args))
                                            :registers regs)
                       :flags access
                       :name (field-name m)
                       :parameter-annotations nil
                       :parameters (coerce (cdr args) 'vector)
                       :return-type ret))
          when (or (find :private access)
                   (find :constructor access))
            collect dcm into direct-methods
          else collect dcm into virtual-methods
          finally (return (list (coerce direct-methods 'vector)
                                (coerce virtual-methods 'vector))))))

(defun link-fields (class)
  (when (fields class)
    (loop for s across (fields class)
          when (find :static (access s))
            collect (make-instance '3b-dex::dex-class-static-field
                                   :value (getf (attributes s) :constant-value)
                                   :annotations nil
                                   :flags (access s)
                                   :name (field-name s)
                                   :type (field-type s))
              into static
          else
            collect (make-instance '3b-dex::dex-class-field
                                   :annotations nil
                                   :flags (access s)
                                   :name (field-name s)
                                   :type (field-type s))
              into instance
          finally (return (list (coerce static 'vector)
                                (coerce instance 'vector))))))

(defun link-dex-class (class-name)
  (flet ((c (c)
           (let ((c (gethash c (native-classes *3bil2-environment*))))
             (assert c)
             c))
         (jn (class)
           (format nil "L~a;" (java-name class))))
    (let* ((class (c class-name))
           (extends (c (extends class)))
           (implements (mapcar #'jn (mapcar #'c (implements class))))
           (methods (link-methods class))
           (fields (link-fields class)))
      (make-instance
       '3b-dex::dex-class
       :annotations nil
       :interfaces (when implements (coerce implements 'vector))
       :superclass (jn extends)
       :type-name (jn class)
       :direct-methods (first methods)
       :virtual-methods (second methods)
       :flags (access class)
       :static-fields (first fields)
       :instance-fields (second fields)))))


(defun link-dex-file (&rest classes)
  (make-instance
   '3b-dex::dex-file
   :endian :le
   :link-table nil
   :maps nil
   :version 35
   :classes (coerce (loop for class in classes
                          collect (link-dex-class class))
                    'vector)))

#++
(with-open-file (f "/tmp/tmp.dex" :direction :io
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :element-type '(unsigned-byte 8))
  (3b-dex::write-dex-file
   (link-dex-file 'com/example/hello:hello-activity
                  'com/example/hello:r$layout
                  'com/example/hello:r$string
                  'com/example/hello:r)
   f))
