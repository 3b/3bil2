(in-package 3bil2)

(defvar *cons-type* "cc.3bb.3bil2.%impl.cons")

(defun ensure-cst (form &key progn)
  (if (typep form 'cst:cst)
      form
      (cst:cst-from-expression (if (and progn (consp form))
                                   (cons 'progn form)
                                   form))))

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

(defmethod cleavir-cst-to-ast:convert-special ((head (eql 'values))
                                               cst
                                               env
                                               (system 3bil2))
  (assert (<= 0 (length (cst:raw (cst:rest cst))) 1))
  (cleavir-cst-to-ast:convert-special
   'cleavir-primop:values cst env system))


(defclass binary-add-ast (cleavir-ast:ast)
  ((args :initarg :args :reader args)))

(cleavir-io:define-save-info binary-add-ast
  (:args args))

(defmethod cleavir-ast:children ((c binary-add-ast))
  (args c))

(defmethod cleavir-cst-to-ast:convert-special ((head (eql '+))
                                               cst
                                               env
                                               (system 3bil2))
  (let* ((args (cleavir-cst-to-ast::convert-sequence (cst:rest cst)
                                                     env system)))
    (make-instance 'binary-add-ast :args args)))



(defmethod cleavir-generate-ast:check-special-form-syntax ((head (eql 'values))
                                                           form)
  (cleavir-code-utilities:check-form-proper-list form))

(defclass native-method-ast (cleavir-ast:function-ast)
  ((native-class :initarg :native-class :reader native-class)
   (arg-types :initarg :arg-types :reader native-method-arg-types)
   (return-type :initarg :return-type :reader return-type)))

(cleavir-io:define-save-info native-method-ast
  (:arg-type native-method-arg-types))

(defvar *current-native-lambda-args*)

(defmethod cleavir-cst-to-ast:convert-special ((symbol (eql 'function))
                                               cst env
                                               (system 3bil2))
  (let ((*current-native-lambda-args* (make-hash-table)))
    (cleavir-cst-to-ast::check-function-syntax cst)
    (cst::db origin (function-cst name-cst) cst
      (declare (ignore function-cst))
      (let ((result
              (if (cleavir-cst-to-ast::proper-function-name-p name-cst)
                  (cleavir-cst-to-ast::convert-named-function name-cst env system)
                  (cleavir-cst-to-ast::convert-lambda-function name-cst env system))))
        (reinitialize-instance result :origin origin)
        (unless (cleavir-cst-to-ast::proper-function-name-p name-cst)
          (let ((arg-types (loop
                             for i in (cleavir-ast:lambda-list result)
                             collect (gethash i *current-native-lambda-args*))))

            (assert (typep result 'cleavir-ast:function-ast))
            (change-class result 'native-method-ast
                          :arg-types arg-types)))
        result))))

(defclass slot-write-ast (cleavir-ast:slot-write-ast)
  ())

(defclass slot-read-ast (cleavir-ast:slot-read-ast)
  ())

(defmethod cleavir-cst-to-ast:convert-special ((symbol (eql '%set-slot-value))
                                               cst env
                                               (system 3bil2))
  (let* ((args (cleavir-cst-to-ast::convert-sequence (cst:rest cst)
                                                     env system))
         (w (cleavir-ast:make-slot-write-ast
             (first args) (second args) (third args))))
    (change-class w 'slot-write-ast)
    ;; (setf (slot-value ...)) expands to this, so return value too
    #++(cleavir-ast:make-progn-ast
        (list w (third args)))
    w))

(defmethod cleavir-cst-to-ast:convert-special ((symbol (eql 'slot-value))
                                               cst env
                                               (system 3bil2))
  (let* ((args (cleavir-cst-to-ast::convert-sequence (cst:rest cst)
                                                     env system))
         (w (cleavir-ast:make-slot-read-ast (first args) (second args))))
    (change-class w 'slot-read-ast)
    w))

(defun canonicalize-type (a)
  (etypecase a
    (symbol a)
    ((cons (eql and))
     (if (eql 2 (length a))
         (canonicalize-type (second a))
         (let ((r (delete-duplicates (mapcar 'canonicalize-type (cdr a)))))
           (if (equal r (cdr a))
               a
               (list* 'and r)))))
    ((cons (member signed-byte unsigned-byte))
     a)
    ((cons (eql vector))
     (let ((c (canonicalize-type (second a))))
       (if (equalp c (second a))
           a
           (list (first a) c))))))

(defmethod cleavir-cst-to-ast:process-parameter
    :around
    ((parameter cst:simple-variable)
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs
     remaining-idspecs-in-group
     remaining-idspecs
     body
     environment
     (system 3bil2))
  (let* ((var (cst:name parameter))
         (raw-var (cst:raw var))
         (origin (cst:source var))
         (type (canonicalize-type
                (cleavir-cst-to-ast::declared-type idspecs))))
    (let ((r (multiple-value-list (call-next-method))))
      (when (boundp '*current-native-lambda-args*)
        (setf (gethash (first (second r)) *current-native-lambda-args*)
              type))
      (values-list r))))

(defmethod cleavir-cst-to-ast::convert-cst
    (cst
     (info 3bil2::native-method-function-info)
     env (system 3bil2))
  (let* ((.this (cst:second cst))
         (super (and (cst:consp .this)
                     (eql 'super (cst:raw (cst:first .this)))))
         (this (if super
                   (cst:second .this)
                   .this))
         (args (cst:rest (cst:rest cst)))
         (argument-asts (cleavir-cst-to-ast::convert-sequence
                         (cst:cons this args)
                         env system)))
    (make-instance 'native-call-ast
                   :name (cst:first cst)
                   :call-type (if super
                                  :invoke-super
                                  :invoke-virtual)
                   :class :todo
                   :argument-asts argument-asts)))


(defclass native-call-instruction (cleavir-ir:funcall-instruction)
  ((name :initarg :name :initform nil :reader native-call-name)
   (call-type :initarg :call-type :initform nil :reader native-call-type)
   (class :initarg :class :initform nil :reader native-call-class)))

(defmethod cleavir-ir-graphviz:label ((i native-call-instruction))
  (format nil "~a:~a"
          (or (native-call-type i) :native-call)
          (native-call-name i)))

(defmethod cleavir-ir:clone-initargs append ((i native-call-instruction))
  (list :name (native-call-name i)
        :call-type (native-call-type i)
        :class (native-call-class i)))

(defmethod cleavir-ast-to-hir:compile-ast ((ast native-method-ast)
                                           context)
  (let ((enter (call-next-method)))
    enter))

(defclass add-instruction (cleavir-ir:instruction
                           cleavir-ir:one-successor-mixin)
  ())

(defmethod cleavir-ast-to-hir:compile-ast ((ast binary-add-ast)
                                           context)
  (let ((temps (cleavir-ast-to-hir::make-temps (args ast))))
    (cleavir-ast-to-hir:compile-arguments
     (args ast)
     temps
     (make-instance 'add-instruction
                    :inputs temps
                    :outputs (cleavir-ast-to-hir::results context)
                    :successors (cleavir-ast-to-hir::successors context))
     (cleavir-ast-to-hir::invocation context))))

#++
(defun compile-arguments (arguments temps successor invocation)
  (loop with succ = successor
	for arg in (reverse arguments)
	for temp in (reverse temps)
        do (format t "~&**~s -> ~s~%" arg temp)
	do (setf succ (cleavir-ast-to-hir:compile-ast
                       arg
		       (cleavir-ast-to-hir:context `(,temp)
					           `(,succ)
					           invocation)))
	finally (return succ)))

(defmethod cleavir-ast-to-hir:compile-ast ((ast native-call-ast)
                                           context)
  (with-accessors ((results cleavir-ast-to-hir::results)
                   (successors cleavir-ast-to-hir::successors))
      context
    (cleavir-ast-to-hir::assert-context ast context nil 1)
    (let* ((all-args (native-call-argument-asts ast))
           (temps (cleavir-ast-to-hir::make-temps all-args)))
      (cleavir-ast-to-hir:compile-arguments
       all-args
       temps
       ;; fixme: make values stuff work right:
       (if (typep results 'cleavir-ir:values-location)
           (let* ((temp (cleavir-ir:new-temporary)))
             (make-instance 'native-call-instruction
                            :name (native-call-name ast)
                            :call-type (native-call-type ast)
                            :class (native-call-class ast)
                            :inputs temps
                            :outputs (list temp)
                            :successors (list
                                         (cleavir-ir:make-fixed-to-multiple-instruction
                                          (list temp) results
                                          (first successors)))))
           (make-instance 'native-call-instruction
                          :name (native-call-name ast)
                          :call-type (native-call-type ast)
                          :class (native-call-class ast)
                          :inputs temps
                          :outputs results
                          :successors successors))
       (cleavir-ast-to-hir::invocation context)))))

(defclass slot-write-instruction (cleavir-ir:slot-write-instruction)
  ((slot-name :initarg :slot-name :reader slot-name)))

(defmethod cleavir-ir:clone-initargs append ((i slot-write-instruction))
  (list :slot-name (slot-name i)))

(defclass slot-read-instruction (cleavir-ir:slot-read-instruction)
  ((slot-name :initarg :slot-name :reader slot-name)))

(defmethod cleavir-ir:clone-initargs append ((i slot-read-instruction))
  (list :slot-name (slot-name i)))

(defmethod cleavir-ast-to-hir:compile-ast ((ast slot-write-ast) context)
  (let ((temp1 (cleavir-ast-to-hir:make-temp))
        (temp3 (cleavir-ast-to-hir:make-temp)))
    (unless (typep (cleavir-ast:form (cleavir-ast:slot-number-ast ast))
                   '(cons (eql quote) (cons symbol)))
      (error "can't compile set-slot-value with variable slot name"))
    (cleavir-ast-to-hir:compile-ast
     (cleavir-ast:object-ast ast)
     (cleavir-ast-to-hir:context
      (list temp1)
      (list
       (cleavir-ast-to-hir:compile-ast
        (cleavir-ast:value-ast ast)
        (cleavir-ast-to-hir:context
         (list temp3)
         (list (make-instance
                'slot-write-instruction
                :inputs (list temp1 temp3)
                :outputs '()
                :slot-name (second (cleavir-ast:form
                                    (cleavir-ast:slot-number-ast ast)))
                :successors (cleavir-ast-to-hir::successors context)))
         (cleavir-ast-to-hir:invocation context))))
      (cleavir-ast-to-hir:invocation context)))))

(defmethod cleavir-ast-to-hir:compile-ast ((ast slot-read-ast) context)
  (let ((temp1 (cleavir-ast-to-hir:make-temp)))
    (unless (typep (cleavir-ast:form (cleavir-ast:slot-number-ast ast))
                   '(cons (eql quote) (cons symbol)))
      (error "can't compile slot-value with variable slot name"))
    (cleavir-ast-to-hir:compile-ast
     (cleavir-ast:object-ast ast)
     (cleavir-ast-to-hir:context
      (list temp1)
      (list (make-instance
             'slot-read-instruction
             :inputs (list temp1)
             :outputs (cleavir-ast-to-hir::results context)
             :slot-name (second (cleavir-ast:form
                                 (cleavir-ast:slot-number-ast ast)))
             :successors (cleavir-ast-to-hir::successors context)))
      (cleavir-ast-to-hir:invocation context)))))

(defmethod cleavir-kildall:transfer
    ((s cleavir-kildall-type-inference::type-inference)
     (instruction native-call-instruction))
  (let ((succ (first (cleavir-ir:successors instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    (cleavir-kildall:with-pool-reader s instruction from
      (cleavir-kildall:copy s succ variable from
          ()
          ((output (cleavir-kildall-type-inference::approximate-type s t)))))))


(defclass asm-ast (cleavir-ast:ast)
  ((code :initarg :code :reader asm-ast-code)
   (result :initarg :result :reader asm-ast-result)))

(cleavir-io:define-save-info asm-ast
  (:code asm-ast-code))

(defmethod cleavir-ast:children ((ast asm-ast))
  (when (asm-ast-result ast)
    (list (asm-ast-result ast))))

(defmethod cleavir-cst-to-ast::convert-special ((head (eql '%asm))
                                                form
                                                env
                                                (system 3bil2))
  (let* ((asm (cst:rest form))
         (ret (cst:first asm))
         (ret1 (cst:raw ret)))
    ;; if first element of body is a symbol, treat it as return value.
    ;; otherwise treat it as asm and we don't return anything
    (if (cst:consp ret)
        (setf ret nil)
        (setf asm (cst:rest asm)))
    (when ret
      (setf ret
            (if (cleavir-env:variable-info env ret)
                (cleavir-cst-to-ast::convert ret env system)
                (cleavir-ast::make-lexical-ast ret1))))
    (setf asm
          (loop for (op . args) in (cst:raw asm)
                ;; fixme: decide which arguments are actually 'evaluated'
                ;; rather than just matching anything in env
                collect
                (cons op
                      (loop for a in args
                            when (cleavir-env:variable-info env a)
                              collect (cleavir-env:identity
                                       (cleavir-env:variable-info env a))
                            else
                              when (and ret (eql a ret1))
                                collect ret
                            else
                              collect a))))
    (make-instance 'asm-ast
                   :code asm
                   :result ret)))


(defclass asm-instruction (cleavir-ir:instruction
                           cleavir-ir:one-successor-mixin
                           cleavir-ast:one-value-ast-mixin)
  ((code :initarg :code :accessor code)))

(defmethod cleavir-ir:clone-initargs append ((i asm-instruction))
  (list :code (code i)))

(defclass asm-input ()
  ((index :initarg :index :reader index)))

(defmethod cleavir-ast-to-hir:compile-ast ((ast asm-ast) context)
  (let* ((%inputs (make-hash-table))
         (index 0)
         (code
           (loop for (op . args) in (asm-ast-code ast)
                 collect
                 (cons op
                       (loop for a in args
                             when (typep a 'cleavir-ast:ast)
                               collect (let ((l (cleavir-ast-to-hir::find-or-create-location
                                                 a)))
                                         (unless (gethash l %inputs)
                                           (setf (gethash l %inputs)
                                                 (make-instance 'asm-input
                                                                :index index))
                                           (incf index))
                                         (gethash l %inputs))
                             else collect a))))
         (inputs (mapcar 'car
                         (sort (alexandria:hash-table-alist %inputs)
                               '< :key (lambda (a) (index (cdr a)))))))
    (format t "~&~%compile asm:~%  ~s~%-> ~s~%"
            code
            (cleavir-ast-to-hir::results context))
    (if (typep (cleavir-ast-to-hir::results context)
               'cleavir-ir:values-location)
        (let* ((temp (cleavir-ir:new-temporary)))
          (make-instance 'asm-instruction
                         :code code
                         :inputs inputs
                         :outputs (list temp)
                         :successors
                         (list
                          (cleavir-ir:make-fixed-to-multiple-instruction
                           (list temp)
                           (cleavir-ast-to-hir::results context)
                           (first
                            (cleavir-ast-to-hir::successors context))))))
        (make-instance 'asm-instruction
                       :code code
                       :inputs inputs
                       :outputs (cleavir-ast-to-hir::results context)
                       :successors
                       (if (null (asm-ast-result ast))
                           (list
                            (first (cleavir-ast-to-hir::successors context)))
                           (list
                            (cleavir-ir:make-assignment-instruction
                             (cleavir-ast-to-hir::find-or-create-location
                              (asm-ast-result ast))
                             (car (cleavir-ast-to-hir::results context))
                             (first (cleavir-ast-to-hir::successors context)))))))))
