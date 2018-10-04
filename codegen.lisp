(in-package 3bil2)

(defvar *current-code* ())
(defvar *variable-type-info* (make-hash-table))
(defvar *compile-env* nil)
(defvar *variable-usage* (make-hash-table))
(defvar *return-type* nil)

(defgeneric hir-to-dalvik (hir))

(defun asm (opcode &rest args)
  (push (cons opcode args) *current-code*))

(defun use-variables (tag vars &key indexed)
  (loop for a in vars
        for i from 0
        do (pushnew (if indexed
                        (list tag i)
                        (list tag))
                    (gethash a *variable-usage*) :test 'equal)))

(defmethod hir-to-dalvik ((ir cleavir-ir:enter-instruction))
  (let ((arguments (cleavir-ir:lambda-list ir))
        (outputs (cleavir-ir:outputs ir)))
    (use-variables :enter arguments :indexed t)
    (format t "enter:~%")
    (format t " <- ~s~%" arguments)
    (format t " -> ~s~%" outputs)
    ;; todo: figure out if we need to process arguments or not?
    (loop for c in (cleavir-ir:successors ir)
          do (hir-to-dalvik c))))

(defmethod clean-type* ((car (eql 'and)) cdr)
  (let ((types (remove-duplicates (mapcar 'clean-type cdr))))
    (if (= 1 (length types))
        (car types)
        (cons 'and types))))

(defmethod clean-type (type)
  (if (consp type)
      (clean-type* (car type) (cdr type))
      type))

(defmethod hir-to-dalvik ((ir cleavir-ir:the-instruction))
  (let* ((input (car (cleavir-ir:inputs ir)))
         (.type (cleavir-ir:value-type ir))
         (type (clean-type .type)))
    (pushnew type
             (gethash input *variable-type-info*)
             :test 'equal)
    (format t "the ~s = ~s~%" input type)
    (hir-to-dalvik (car (cleavir-ir:successors ir)))))

(defun java-type-string-for-class (class)
  (etypecase class
    ((cons (eql signed-byte))
     (ecase (second class)
       (32 "I")))
    ((or (signed-byte 32) (eql fixnum))
     "I")
    ((eql :void)
     "V")
    ((or symbol native-class)
     (when (symbolp class)
       (setf class (gethash class (native-classes *compile-env*))))
     (format nil "L~a;" (java-name class)))))

(defun constant-assignment-p (ir)
  ;; fixme: probably should work with NIL literal too
  (when (and (typep ir 'cleavir-ir:assignment-instruction)
             (typep (car (cleavir-ir:inputs ir))
                    'cleavir-ir:load-time-value-input))
    (let ((f (cleavir-ir:form (car (cleavir-ir:inputs ir)))))
      (if (typep f '(cons (eql quote)))
          (second f)
          f))))

(defun java-type-string-for-variable (var)
  (etypecase var
    (cleavir-ir:lexical-location
     (let ((c (car (gethash var *variable-type-info*))))
       (when (and (not c)
                  (= 1 (length (cleavir-ir:defining-instructions var)))
                  (constant-assignment-p
                   (car (cleavir-ir:defining-instructions var))))
         (setf c (constant-assignment-p
                  (car (cleavir-ir:defining-instructions var)))))
       (format t "var ~s -> ~s~%" var c)
       (java-type-string-for-class c)))))

(defun simplified-signature (sig)
  (assert (char= #\( (char sig 0)))
  (loop with skip = nil
        for i from 1 below (length sig)
        for c = (char sig i)
        until (char= c #\))
        unless skip
          collect c into args
        when (char= c #\L)
          do (setf skip t)
        when (char= c #\;)
          do (setf skip nil)
        finally (return (list (format nil "~a~{~a~}" (char sig (1+ i))
                                      (reverse args))
                              (format nil "~a" (char sig (1+ i)))))))

(defun select-method-signature (method class args)
  (let* ((signatures (signatures-for-class class method))
         (arg-types (mapcar #'java-type-string-for-variable args))
         (arg-signature (format nil "(~{~a~})" arg-types))
         (match (loop for s in (alexandria:hash-table-keys signatures)
                      when (alexandria:starts-with-subseq arg-signature s)
                        collect s)))
    (format t " sigs ~s~%  @@ ~s~%" arg-signature
            (alexandria:hash-table-keys signatures))
    (format t " match = ~s~%" match)
    (unless (= 1 (length match))
      (format t "couldn't find signature match for ~s? ~s ~s~%"
              (name method)
              arg-signature
              (alexandria:hash-table-keys signatures)))
    (append (simplified-signature (first match))
            (list
             (map 'vector #'java-type-string-for-variable
                  args)
             (field-name method)
             nil))))

(defmethod hir-to-dalvik ((ir native-call-instruction))
  (let* ((call-type (native-call-type ir))
         (this (first (cleavir-ir:inputs ir)))
         (args (rest (cleavir-ir:inputs ir)))
         (class-name (first (gethash this *variable-type-info*)))
         (class (gethash class-name (native-classes *compile-env*)))
         (super-name (extends class))
         (super (when super-name
                  (gethash super-name (native-classes *compile-env*))))
         (name (if (typep (native-call-name ir) 'cst:cst)
                   (cst:raw (native-call-name ir))
                   (native-call-name ir)))
         (method (gethash name (native-methods *compile-env*))))
    (assert (= 1 (length (gethash this *variable-type-info*))))
    (assert class)
    (format t "call (~s ~s ~s)~%" name this args)
    (format t " class = ~s (~s)~%" class class-name)
    (format t " extends = ~s~%" super)
    (format t " method = ~s (~s)~%" method name)
    (use-variables :native-call (cleavir-ir:inputs ir) :indexed nil)
    (ecase call-type
      (:invoke-direct
       (error ":invoke-direct todo")
       #++(asm invoke-direct (extends class)))
      (:invoke-super
       (apply #'asm
              ;; special case constructor
              (if (eq name 'java/lang/object:<init>)
                  :invoke-direct
                  :invoke-super)
              (list
               (java-type-string-for-class (extends class))
               (select-method-signature method super-name args))
              this
              args))
      (:invoke-virtual
       (apply #'asm
              :invoke-virtual
              (list (list
                     (java-type-string-for-class class)
                     (select-method-signature method class-name args)))
              this
              args)))
    (hir-to-dalvik (car (cleavir-ir:successors ir)))))

(defmethod hir-to-dalvik ((ir cleavir-ir:assignment-instruction))
  (let ((in (car (cleavir-ir:inputs ir)))
        (out (car (cleavir-ir:outputs ir))))
    (format t "assign ~s <- ~s~%" out in)
    (etypecase in
      (cleavir-ir:load-time-value-input
       (let* ((form (cleavir-ir:form in))
              (v (if (consp form) (second form) form)))
         (assert (typep form
                        '(or number (cons (eql quote) (cons number)))))
         (use-variables :assign (cleavir-ir:outputs ir)
                        :indexed t)
         (asm :const out v)
         (setf (gethash out *variable-type-info*)
               (cond
                 ((typep v '(signed-byte 32))
                  '((signed-byte 32)))
                 (t
                  (error "don't know how to determine type of ~s yet" v))))))))
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:fixed-to-multiple-instruction))
  (format t "fixed-to-multiple-instruction <- ~s~%" (cleavir-ir:inputs ir))
  (format t " -> ~s~%" (cleavir-ir:outputs ir))
  (setf (gethash (car (cleavir-ir:outputs ir)) *variable-type-info*)
        (gethash (car (cleavir-ir:inputs ir)) *variable-type-info*))

  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:return-instruction))
  (format t "return ~s~%" (cleavir-ir:inputs ir))
  (let* ((v (car (cleavir-ir:inputs ir)))
         (d (cleavir-ir:defining-instructions v)))
    (format t "  ~s ~s~%" v d)
    (if (and (= 1 (length d))
             (typep (car d) 'cleavir-ir:fixed-to-multiple-instruction)
             (zerop (length (cleavir-ir:inputs (car d)))))
        (progn
          (assert (or (not *return-type*)
                      (eql *return-type* :void)))
          (setf *return-type* :void)
          (asm :return-void))
        (let ((type (car (gethash v *variable-type-info*))))
          (unless type
            (break "~s" v
                   *variable-type-info*
                   *variable-usage*))
          (assert (or (not *return-type*)
                      (eql *return-type* type)))
          (setf *return-type* type)
          (use-variables :return (cleavir-ir:inputs ir) :indexed nil)
          (asm :return v)))))



(defun regalloc (uses code)
  (let ((alloc (make-hash-table))
        (counter -1)
        (max 0)
        (args))
    (loop for v being the hash-keys of uses
            using (hash-value u)
          for e = (assoc :enter u)
          do (format t "~s ~s~%   ~s~%" v u e)
          when e
            do (push (list v (second e)) args)
          else do (format t "alloc ~s ~s~%" v counter)
                  (setf (gethash v alloc)
                        (or (gethash v alloc) (incf counter))))
    (incf counter)
    (loop for (v i) in args
          do (assert (not (gethash v alloc)))
             (setf max (max max (+ i counter)))
             (setf (gethash v alloc) (+ i counter)))
    (values
     (loop for inst in code
           collect (loop for i in inst
                         when (gethash i alloc)
                           collect it
                         else collect i))
     (length args)
     (1+ max)
     (format nil "(~{~a~})~a"
             ;; don't include 'this' in signature
             (loop for (a) in (cdr (sort (copy-list args)
                                     '< :key 'second))
                   collect (java-type-string-for-variable a))
             (java-type-string-for-class  *return-type*)))))

(defun compile-hir (hir env)
  (let ((*variable-type-info* (make-hash-table))
        (*current-code* nil)
        (*compile-env* env)
        (*variable-usage* (make-hash-table))
        (*return-type* nil))
    (hir-to-dalvik hir)
    (setf *current-code* (reverse *current-code*))
    (format t "~%~%compiled ~s~%" *current-code*)
    (format t "~%~%regalloc:~%~{  ~a ~a~%~}"
            (alexandria:hash-table-plist *variable-usage*))
    (regalloc *variable-usage* *current-code*)))
