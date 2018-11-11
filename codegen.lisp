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

(defmethod clean-type* (car cdr)
  (cons car cdr))

(defmethod clean-type (type)
  (if (consp type)
      (clean-type* (car type) (cdr type))
      type))

(defmethod hir-to-dalvik ((ir cleavir-ir:the-instruction))
  (let* ((input (car (cleavir-ir:inputs ir)))
         (.type (cleavir-ir:value-type ir))
         (type (clean-type .type))
         (old (gethash input *variable-type-info*)))
    #++(when (gethash input *variable-type-info*)
         (break "the ~s -> ~s~%"
                (gethash input *variable-type-info*)
                type))
    #++
    (pushnew type
             (gethash input *variable-type-info*)
             :test 'equal)
    (format t "the ~s = ~s~%" input type)
    (cond
      ((and old (not (eql old type)))
       (asm :check-cast input
            (java-type-string-for-class type)
            #++(let ((c (gethash type (native-classes *3bil2-environment*))))
                 (if c
                     (format nil "L~a;" (java-name c)))))
       (setf (gethash input *variable-type-info*) type))
      ((not old)
       (setf (gethash input *variable-type-info*) type))
      ((eql type old) ;; do nothing
       )
      (t
       (break "todo? ~s ~s" old type)))

    (hir-to-dalvik (car (cleavir-ir:successors ir)))))

(defun java-type-string-for-class (class)
  (etypecase class
    ((cons (eql signed-byte))
     (ecase (second class)
       (32 "I")))
    ((or (signed-byte 32) (eql fixnum))
     "I")
    ((eql single-float)
     "F")
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

(defun get-type-for-variable (var)
  (etypecase var
    (cleavir-ir:lexical-location
     (let ((c (gethash var *variable-type-info*)))
       (when (and (not c)
                  (= 1 (length (cleavir-ir:defining-instructions var)))
                  (constant-assignment-p
                   (car (cleavir-ir:defining-instructions var))))
         (setf c (constant-assignment-p
                  (car (cleavir-ir:defining-instructions var)))))
       (format t "var ~s -> ~s~%" var c)
       c))
    (cons
     var)
    (symbol
     var)))

(defun java-type-string-for-variable (var)
  (java-type-string-for-class (get-type-for-variable var)))

(defun simplified-signature (sig)
  (3b-dex::simplified-signature sig))

(defun split-arg-types (sig)
  (assert (char= #\( (char sig 0)))
  (let ((args nil))
    (loop with skip = nil
          for i from 1 below (length sig)
          for c = (char sig i)
          until (char= c #\))
          when (and (not skip) (char= c #\L))
            do (setf skip t)
               (push nil args)
          unless skip
            do (push (string c) args)
          when (char= c #\;)
            do (setf skip nil)
            and do (setf (car args)
                         (coerce (reverse (cons #\; (car args))) 'string))
          when skip
            do (push c (car args)))
    (reverse args)))

(defun match-signature (args signature)
  #++(break "~a ~a" args arg-types)
  (format t "match ~s (~a) / ~s~%" args
          (mapcar 'get-type-for-variable args)
          signature)
  (let ((arg-types (split-arg-types signature)))
    (when (= (length args) (length arg-types))
      (loop for a in args
            for vt = (get-type-for-variable a)
            for at in arg-types
            do (format t "@@ ~s / ~s ? ~s~%" a
                       (java-type-string-for-variable a) at)
            collect (if (string= (java-type-string-for-variable a) at)
                        (java-type-string-for-variable a)
                        (when (or (symbolp vt)
                                  (typep vt 'native-class))
                          (loop for i in (cpl-cache vt)
                                for ji = (java-type-string-for-class i)
                                do (format t " @s@ ~s / ~s ? ~s~%"
                                           i ji at)
                                when (string= ji at)
                                  return ji)))
              into ret
            finally (unless (position nil ret)
                      (return (list signature ret)))))))

(defun select-method-signature (method class args)
  (let* ((signatures (signatures-for-class class method))
         #++(arg-types (mapcar #'java-type-string-for-variable args))
         #++(arg-signature (format nil "(~{~a~})" arg-types))
         (static nil)
         (match (if signatures
                    (loop for s in (alexandria:hash-table-keys signatures)
                          when (match-signature args s)
                            collect it)
                    (let ((s (loop for s in (alexandria:hash-table-keys
                                             (gethash (native-class method)
                                                      (signatures method)))
                                   when (match-signature (cons class args) s)
                                     collect it)))
                      (when s (setf static t))
                      s))))
    #++(format t " sigs ~s~%  @@ ~s~%" arg-signature
               (alexandria:hash-table-keys signatures))
    (format t " match = ~s~%" match)
    (unless (= 1 (length match))
      (format t "couldn't find signature match for ~s? ~s ~s~%"
              (cleavir-env:name method)
              nil
              (alexandria:hash-table-keys signatures)))
    (values
     `((,@(simplified-signature (first (first match)))
        ,(coerce (second (first match)) 'vector))
       ,(field-name method)
       nil)
     (first (first match))
     (if static (native-class method) class)
     static)))

(defun find-native-class (java-name)
  (let* ((n (subseq java-name 1 (1- (length java-name))))
         (pos (position #\/ n :from-end t))
         (p (subseq n 0 pos))
         (s (subseq n (1+ pos)))
         (sym (find-symbol (string-upcase s)
                           (find-package (string-upcase p)))))
    sym
    #++(when sym (gethash sym (native-classes *3bil2-environment*)))))

(defmethod hir-to-dalvik ((ir native-call-instruction))
  (let* ((call-type (native-call-type ir))
         (this (first (cleavir-ir:inputs ir)))
         (args (rest (cleavir-ir:inputs ir)))
         (class-name (gethash this *variable-type-info*))
         (class (gethash class-name (native-classes *compile-env*)))
         (super-name (when class (extends class)))
         (super (when super-name
                  (gethash super-name (native-classes *compile-env*))))
         (name (if (typep (native-call-name ir) 'cst:cst)
                   (cst:raw (native-call-name ir))
                   (native-call-name ir)))
         (method (gethash name (native-methods *compile-env*)))
         (sig nil))
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
              (list* (java-type-string-for-class (extends class))
                     (setf sig
                           (select-method-signature method super-name args)))
              this
              args))
      (:invoke-virtual
       (multiple-value-bind (s1 s2 class-name static)
           (select-method-signature method class-name args)
         (setf class (gethash class-name (native-classes *compile-env*)))
         (setf sig s1)
         (format t "sig = ~s ~%" sig)
         (format t "= ~s~%" (gethash class-name (signatures method)))
         (let ((access (third
                        (gethash s2 (gethash class-name (signatures method))))))
           (format t " access= ~s~%" access)
           (apply #'asm
                  (if static
                      :invoke-static
                      :invoke-virtual)
                  (list* (java-type-string-for-class class)
                         sig)
                  this
                  args)))))
    (when (and (cleavir-ir:outputs ir)
               sig
               (cleavir-ir:using-instructions (first (cleavir-ir:outputs ir))))
      (let ((r (second (first sig)))
            (out (first (cleavir-ir:outputs ir))))
        (format t "outputs = ~s~%" (cleavir-ir:outputs ir))
        (format t "sig = ~s~%" sig)
        (format t "r = ~s~%" r)
        (ecase (char r 0)
          (#\V ;; void ret, do nothing
           )
          (#\L ;; object return
           (asm :move-result-object out)
           ;; and set type so we can do casts if needed before using
           (when (gethash out *variable-type-info*)
             (break "return value: type info = ~s -> ~s~%"
                    r (gethash out *variable-type-info*)))
           (setf (gethash out *variable-type-info*) (find-native-class r)))
          (#\I ;; int return
           (asm :move-result (first (cleavir-ir:outputs ir)))))))

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
                        '(or number (cons (eql quote) (cons number))
                          string (cons (eql quote) (cons string)))))
         (use-variables :assign (cleavir-ir:outputs ir)
                        :indexed t)
         (etypecase v
           ;; todo: check these types...
           ((or (signed-byte 32) (unsigned-byte 32) single-float)
            (if (typep v 'float)
                (asm :const out (ieee-floats:encode-float32 v))
                (asm :const out v)))
           ((or (signed-byte 64) (unsigned-byte 674) double-float)
            (asm :const-wide out v))
           (string
            (asm :const-string out v)))
         (setf (gethash out *variable-type-info*)
               (cond
                 ((typep v '(signed-byte 32))
                  '(signed-byte 32))
                 ((typep v 'string)
                  'java/lang:string)
                 ((typep v 'single-float)
                  'single-float)
                 (t
                  (error "don't know how to determine type of ~s yet" v))))))))
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:fixed-to-multiple-instruction))
  (format t "fixed-to-multiple-instruction <- ~s~%" (cleavir-ir:inputs ir))
  (format t " -> ~s~%" (cleavir-ir:outputs ir))
  (setf (gethash (car (cleavir-ir:outputs ir)) *variable-type-info*)
        (gethash (car (cleavir-ir:inputs ir)) *variable-type-info*))

  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:multiple-to-fixed-instruction))
  (format t "multiple-to-fixed-instruction <- ~s~%" (cleavir-ir:inputs ir))
  (format t " -> ~s~%" (cleavir-ir:outputs ir))
  (when (cleavir-ir:outputs ir)
    (error "todo: multiple-to-fixed-instruction not implemented yet"))

  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:nop-instruction))
  (format t "nop~%")
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir add-instruction))
  (format t "add~%")
  (when (cleavir-ir:outputs ir)
    (assert (= 2 (length (cleavir-ir:inputs ir))))
    (use-variables :add (cleavir-ir:inputs ir))
    (asm :add-int (first (cleavir-ir:outputs ir))
         (first (cleavir-ir:inputs ir))
         (second (cleavir-ir:inputs ir))))
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:slot-write-instruction))
  (format t "write slot ~s~%" (slot-name ir))
  (let* ((object (first (cleavir-ir:inputs ir)))
         (value (second (cleavir-ir:inputs ir)))
         (class (gethash (gethash object *variable-type-info*)
                         (native-classes *3bil2-environment*)))
         (slot (find (slot-name ir) (fields class) :key 'name))
         (slot-id (list (java-type-string-for-class class)
                        (field-name slot)
                        (field-type slot))))
    (assert slot)
    (use-variables :write-slot (list value object))
    (format t "class ~s / ~s " (gethash object *variable-type-info*)
            slot)

    (ecase (char (field-type slot) 0) ;; todo: other types
      (#\I
       (asm :iput value object slot-id))
      (#\L
       (asm :iput-object value object slot-id))))
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:slot-read-instruction))
  (format t "read slot ~s~%" (slot-name ir))
  (let* ((object (first (cleavir-ir:inputs ir)))
         (class (gethash (gethash object *variable-type-info*)
                         (native-classes *3bil2-environment*)))
         (slot (find (slot-name ir) (fields class) :key 'name))
         (slot-id (list (java-type-string-for-class class)
                        (field-name slot)
                        (field-type slot)))
         (out (first (cleavir-ir:outputs ir))))
    (assert slot)
    (use-variables :read-slot (list object))
    (format t "class ~s / ~s " (gethash object *variable-type-info*)
            slot)

    (ecase (char (field-type slot) 0) ;; todo: other types
      (#\I
       (asm :iget out object slot-id))
      (#\L
       (asm :iget-object out object slot-id))))
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
        (let ((type (gethash v *variable-type-info*)))
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
     (loop for (i) in (sort (copy-list args) '< :key 'second)
           collect (java-type-string-for-variable i))
     (1+ max)
     (format nil "(~{~a~})~a"
             ;; don't include 'this' in signature
             (loop for (a) in (cdr (sort (copy-list args)
                                         '< :key 'second))
                   collect (java-type-string-for-variable a))
             (java-type-string-for-class *return-type*))
     (java-type-string-for-class *return-type*))))

(defun compile-hir (hir env &optional arg-types)
  (format t "~&compile-hir~s~%" hir)
  (let ((*variable-type-info* (make-hash-table))
        (*current-code* nil)
        (*compile-env* env)
        (*variable-usage* (make-hash-table))
        (*return-type* nil))
    (when arg-types
      (loop for i in (cleavir-ir:lambda-list hir)
            for a in arg-types
            when a
              do (setf (gethash i *variable-type-info*) a)))
    (hir-to-dalvik hir)
    (setf *current-code* (reverse *current-code*))
    (format t "~%~%compiled ~s~%" *current-code*)
    (format t "~%~%regalloc:~%~{  ~a ~a~%~}"
            (alexandria:hash-table-plist *variable-usage*))
    (regalloc *variable-usage* *current-code*)))
