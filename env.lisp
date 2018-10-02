#++(require '3bil2)
(in-package 3bil2)

(defclass 3bil2-environment (sicl-simple-environment:simple-environment)
  ;; map of list(set?) of types -> class name for closure types
  (#++(closure-types :initform (make-hash-table) :reader closure-types)
   (native-classes :initform (make-hash-table) :reader native-classes)
   (native-methods :initform (make-hash-table) :reader native-methods)
   (native-slots :initform (make-hash-table) :reader native-slots)
   ;; static non-const fields, accessible as foo/bar/class-name:*field*
   (native-globals :initform (make-hash-table) :reader native-globals)
   (constants :initform (make-hash-table) :reader constants)))

(defclass native-method-function-info (cleavir-env:special-operator-info)
  ( ;; lisp name of native class
   (native-class :reader native-class :initarg :native-class)
   (from :reader from :initarg :from)
   ;; java name of field
   (field-name :reader field-name :initarg :field-name)
   (signatures :accessor signatures :initarg :signatures)))

(defun define-constant (name value)
  (when (nth-value 1 (gethash name (constants *3bil2-environment*)))
    ;; fixme: handle constants properly
    #++(unless (eql value (gethash name (constants *3bil2-environment*)))
      (format t "~&!!!!! redefining constant ~s from ~s to ~s!~%"
              name (gethash name (constants *3bil2-environment*)) value)))
  (setf (gethash name (constants *3bil2-environment*))
        value))

(defclass native-class ()
  ((name :reader name :initarg :name)
   (java-name :Reader java-name :initarg :java-name)
   (extends :reader extends :initarg :extends)
   (implements :reader implements :initarg :implements)
   (fields :reader fields :initarg :fields)
   (methods :reader methods :initarg :methods)
   (attributes :reader attributes :initarg :attributes)
   (access :reader access :initarg :access)))

(defclass native-global ()
  ((name :reader name :initarg :name)
   (from :reader from :initarg :from)
   ;; lisp name of native class
   (native-class :reader native-class :initarg :native-class)
   ;; java name of field
   (field-name :reader field-name :initarg :field-name)
   (field-type :reader field-type :initarg :type)
   (access :reader access :initarg :access)))

(defclass native-slot ()
  ((name :reader name :initarg :name)
   (from :reader from :initarg :from)
   ;; lisp name of native class
   (native-class :reader native-class :initarg :native-class)
   ;; java name of field
   (field-name :reader field-name :initarg :field-name)
   (field-type :reader field-type :initarg :type)
   (access :reader access :initarg :access)
   (attributes :reader attributes :initarg :attributes)))


(defmethod cleavir-compilation-policy:compute-policy-quality
    (name optimize (env 3bil2-environment))
  ;; todo: EQL specialize on known qualities:
  #++ CLEAVIR-ESCAPE:TRUST-DYNAMIC-EXTENT
  #++ CLEAVIR-KILDALL-TYPE-INFERENCE:INSERT-TYPE-CHECKS
  ;;(format t "~%got optimize quality ~s~%  ~s?" name optimize)
  1)


(defvar *3bil2-environment* (make-instance '3bil2-environment))
(setf (sicl-genv:declaration 'values *3bil2-environment*) t)
(setf (sicl-genv:declaration 'native-method *3bil2-environment*) t)

(defmethod cleavir-env:function-info ((env 3bil2-environment) name)
  (or (call-next-method)
      (when (symbolp name)
        (cond
          ((or (member name '(values))
               (special-operator-p name))
           (make-instance 'cleavir-env:special-operator-info
                          :name name))
          ((gethash name (native-methods *3bil2-environment*))
           (format t "~&!!! ~s ~s~%" name
                   (gethash name (native-methods *3bil2-environment*)))
           ;; return value from hash table
                                        ;(break ",jhvb")
           (gethash name (native-methods *3bil2-environment*)))
          ((eq (symbol-package name) (find-package 'cleavir-primop))
           (make-instance 'cleavir-env:special-operator-info
                          :name name))
          ((member name '(+ eq error))
           (make-instance 'cleavir-env:global-function-info
                          :name name))))))

(defmethod cleavir-env:type-expand ((env 3bil2-environment) type)
  type)
(defmethod cleavir-env:has-extended-char-p ((env 3bil2-environment))
  nil)
(defmethod cleavir-env:float-types ((env 3bil2-environment))
  '(single-float double-float))
(defmethod cleavir-env:upgraded-array-element-types ((env 3bil2-environment))
  '(single-float double-float character (unsigned-byte 8) (unsigned-byte 32)))
(defmethod cleavir-env:upgraded-complex-part-types ((env 3bil2-environment))
  '(single-float double-float))

(defun register-ffi-method (lisp-name &key name type access attributes
                                        native-class from)
  ;; remove some things we don't care about, so we can check for
  ;; changes in other parts
  (setf access (remove :native access))
  (setf attributes (remove :code attributes :key 'car))
  (if (gethash lisp-name (native-methods  *3bil2-environment*))
      (let ((old (gethash lisp-name (native-methods  *3bil2-environment*))))
        (pushnew (list native-class type access attributes)
                 (signatures old) :test 'equal)
        old)
      (setf (gethash lisp-name (native-methods *3bil2-environment*))
            (make-instance 'native-method-function-info
                           :name lisp-name
                           :from (first (first from))
                           :native-class native-class
                           :field-name name
                           :signatures (list (list native-class type access
                                                   attributes))))))

(defun register-ffi-methods (methods class)
  (loop for (n i) on methods by #'cddr
        collect (apply #'register-ffi-method n :native-class class i)))

(defun register-ffi-field (lisp-name &key name type access attributes
                                       native-class from)
  ;; expose static fields as +constants+ or *globals*
  (when (member :static access)
    (when (and (assoc :constant-value attributes)
               (not (member :final access)))
      ;; todo: figure out how to handle this if it ever happens
      (break "static+constant-value but not final? ~s"
             (list lisp-name name type access attributes)))
    (if (and (assoc :constant-value attributes)
             (member :final access))
        (define-constant lisp-name (cadr (assoc :constant-value attributes)))
        (setf (gethash lisp-name (native-globals *3bil2-environment*))
              (make-instance 'native-global
                             :name lisp-name
                             :from (first from)
                             :native-class native-class
                             :field-name name
                             :type type
                             :access access)))
    ;; register it as a slot name either way, though not sure if
    ;; slot-value will actually work on static slots any time soon
    (setf (gethash lisp-name (native-slots *3bil2-environment*))
          (make-instance 'native-slot
                         :name lisp-name
                         :from (first (first from))
                         :native-class native-class
                         :field-name name
                         :type type
                         :access access
                         :attributes attributes))))

(defun register-ffi-fields (fields class)
  (loop for (n i) on fields by #'cddr
        collect (apply #'register-ffi-field n :native-class class i)))

(defun register-ffi-class (class-name &key name extends implements
                                        methods fields
                                        access attributes)
  (setf (gethash class-name (native-classes *3bil2-environment*))
        (make-instance 'native-class
                       :name class-name
                       :java-name name
                       :extends extends
                       :implements implements
                       :methods (register-ffi-methods methods class-name)
                       :fields (register-ffi-fields fields class-name)
                       :attributes attributes
                       :access access)))
