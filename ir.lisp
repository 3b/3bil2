(in-package 3bil2)

(defvar *cons-type* "cc.3bb.3bil2.%impl.cons")


(defclass native-call-ast (cleavir-ast:ast)
  ((name :initarg :name :reader native-call-name)
   (call-type :initarg :call-type :reader native-call-type)
   (class :initarg :class :reader native-call-class)
   (argument-asts :initarg :argument-asts :reader native-call-argument-asts)))

(defun make-native-call-ast (class function-name argument-asts
                             &key origin (policy cleavir-ast:*policy*)
                               call-type)
  (make-instance 'native-call-ast
                 :origin origin :policy policy
                 :name function-name
                 :class class
                 :argument-asts argument-asts
                 :call-type call-type))

(cleavir-io:define-save-info native-call-ast
  (:argument-asts native-call-arguments)
  (:name native-call-name)
  (:call-type native-call-type)
  (:class native-call-class))

(defmethod cleavir-ast:children ((ast native-call-ast))
  (native-call-argument-asts ast))

(defmethod cleavir-generate-ast:convert-special ((head (eql 'values))
                                                 form
                                                 env
                                                 (system 3bil2))
  (assert (zerop (length (rest form))))
  (cleavir-ast:make-values-ast nil))

(defmethod cleavir-generate-ast:check-special-form-syntax ((head (eql 'values))
                                                           form)
  (cleavir-code-utilities:check-form-proper-list form))

(defclass native-method-ast (cleavir-ast:function-ast)
  ((native-class :initarg :native-class :reader native-class)
   (return-type :initarg :return-type :reader return-type)))

(defmethod cleavir-generate-ast:convert-code (lambda-list body env system
                                              &key
                                                (block-name nil block-name-p))
  (let* ((parsed-lambda-list
	   (cleavir-code-utilities:parse-ordinary-lambda-list lambda-list))
	 (required (cleavir-code-utilities:required parsed-lambda-list)))
    (multiple-value-bind (declarations documentation forms)
	(cleavir-code-utilities:separate-function-body body)
      ;; FIXME: Handle documentation
      (declare (ignore documentation))
      (let* ((declarations (reduce #'append (mapcar #'cdr declarations)))
             (canonicalized-dspecs
               (cleavir-code-utilities:canonicalize-declaration-specifiers
                declarations
                (cleavir-env:declarations env)))
             (native-class (cadr (assoc 'native-method declarations)))
             (.values (assoc 'values declarations))
             (values (when .values (or (cdr .values) :void))))
        (unless values
          ;;todo: check for method in superclass, and copy return type
          ;;from that.  possibly error if not?
          (error "need to specify return type?"))
	(multiple-value-bind (idspecs rdspecs)
	    (cleavir-generate-ast::itemize-declaration-specifiers
	     (cleavir-generate-ast::itemize-lambda-list parsed-lambda-list)
	     canonicalized-dspecs)
	  (multiple-value-bind (ast lexical-lambda-list)
	      (cleavir-generate-ast::process-required
               required parsed-lambda-list idspecs
               (cleavir-generate-ast::make-body rdspecs forms
                                                block-name block-name-p)
               env
               system)
            (let ((f (cleavir-ast:make-function-ast ast lexical-lambda-list)))
              (if native-class
                  (change-class f 'native-method-ast
                                :native-class native-class
                                :return-type values)
                  f))))))))

(defmethod cleavir-generate-ast::convert-form
    (form (info 3bil2::native-method-function-info)
     env (system 3bil2))
  (let* ((.this (second form))
         (super (and (consp .this)
                     (eql 'super (first .this))))
         (this (if super
                   (second .this)
                   .this))
         (args (cddr form))
         (argument-asts (cleavir-generate-ast:convert-sequence
                         (list* this args)
                         env system)))
    (make-instance 'native-call-ast
                   :name (car form)
                   :call-type (if super
                                  :invoke-super
                                  :invoke-virtual)
                   :class :todo
                   :argument-asts argument-asts)))


(defclass native-call-instruction (cleavir-ir:funcall-instruction)
  ((name :initarg :name :reader native-call-name)
   (call-type :initarg :call-type :reader native-call-type)
   (class :initarg :class :reader native-call-class)))

(defmethod cleavir-ir-graphviz:label ((i native-call-instruction))
  (format nil "~a:~a"
          (or (native-call-type i) :native-call)
          (native-call-name i)))


(defmethod cleavir-ast-to-hir:compile-ast ((ast native-call-ast)
                                           context)
  (with-accessors ((results cleavir-ast-to-hir::results)
		   (successors cleavir-ast-to-hir::successors))
      context
    (cleavir-ast-to-hir::assert-context ast context nil 1)
    (let* ((all-args (native-call-argument-asts ast))
	   (temps (cleavir-ast-to-hir::make-temps all-args)))
      (cleavir-ast-to-hir::compile-arguments
       all-args
       temps
       ;; fixme: make values stuff work right:
       (if (typep results 'cleavir-ir:values-location)
	   (make-instance 'native-call-instruction
                          :name (native-call-name ast)
                          :call-type (native-call-type ast)
                          :class (native-call-class ast)
                          :inputs temps
                          :outputs (list results)
                          :successors successors)
	   (let* ((values-temp (make-instance 'cleavir-ir:values-location)))
             (make-instance 'native-call-instruction
                            :name (native-call-name ast)
                            :call-type (native-call-type ast)
                            :class (native-call-class ast)
                            :inputs temps
                            :outputs (list values-temp)
                            :successors
                            successors)))
       (cleavir-ast-to-hir::invocation context)))))


