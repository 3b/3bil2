(in-package 3bil2)

(defvar *current-code* ())
(defvar *variable-type-info* (make-hash-table))
(defvar *compile-env* nil)
(defvar *variable-usage* (make-hash-table))
(defvar *return-type* nil)
(defvar *outs* 0)

(defgeneric hir-to-dalvik (hir))

(defmethod hir-to-dalvik :before (ir)
  (format t "compile ~s~%" ir))

(defun set-variable-type (v type)
  (when (stringp type)
    (let ((nc (find-class-by-java-name type)))
      (when nc
        (setf type (name nc)))))
  (format t "  set type ~s -> ~s~%"
          v type)
  (when (stringp type)
    (error "couldn't find class for type ~s?" type))
  #++
  (unless v
    (break "no var?"))
  #++
  (unless type
    (break "no type? ~s" v))
  (setf (gethash v *variable-type-info*) type))

(defun get-variable-type (v &key errorp)
  (let ((r (gethash v *variable-type-info*)))
    (format t "   get type ~s == ~s~%" v r)
    #++
    (unless v
      (break "no var?"))
    (when (and errorp (not r))
      (break "no type set for variable ~s?" v))
    r))

(defun asm (opcode &rest args)
  ;; track side effects of some ops
  (labels ((set-type (&optional type)
             (set-variable-type (first args) (or type (second args))))
           (copy-type ()
             (set-type (get-variable-type (second args))))
           (check (t1 &optional t2)
             (assert (eql (get-variable-type (first args)) t1))
             (when t2
               (assert (eql (get-variable-type (second args)) t2))))
           (set/check (t1 t2 &optional t3)
             (set-type t1)
             (assert (eql (get-variable-type (second args)) t2))
             (when t3
               (assert (eql (get-variable-type (third args)) t3)))))
    (case opcode
      ((:new-instance :const-class)
       (set-type))
      ((:move
        :move/from16 :move/16
        :move-wide :move-wide/from16 :move-wide/16
        :move-object :move-object/from16 :move-object/16)
       (copy-type))
      #++
      ((:const-string :const-string/jumbo))
      ((:if-eq
        :if-ne :if-lt :if-ge :if-gt :if-le
        :if-eqz :if-nez :if-ltz :if-gez :if-gtz :if-lez)
       (set-type ;; fixme: correct type?
        :boolean))
      ((:neg-int :not-int)
       (set/check '(signed-byte 32) '(signed-byte 32)))
      ((:neg-long :not-long)
       (set/check '(signed-byte 64) '(signed-byte 64)))
      (:neg-float
       (set/check 'single-float 'single-float))
      (:neg-double
       (set/check 'double-float 'double-float))

      (:int-to-long
       (set/check '(signed-byte 64) '(signed-byte 32)))
      (:int-to-float
       (set/check 'single-float '(signed-byte 32)))
      (:int-to-double
       (set/check 'double-float '(signed-byte 32)))
      (:int-to-byte
       (set/check '(signed-byte 8) '(signed-byte 32)))
      (:int-to-char
       (set/check 'character '(signed-byte 32)))
      (:int-to-short
       (set/check '(signed-byte 16) '(signed-byte 32)))

      (:long-to-int
       (set/check '(signed-byte 32) '(signed-byte 64)))
      (:long-to-float
       (set/check 'single-float '(signed-byte 64)))
      (:long-to-double
       (set/check 'double-float '(signed-byte 64)))

      (:float-to-int
       (set/check '(signed-byte 32) 'single-float))
      (:float-to-long
       (set/check '(signed-byte 64) 'single-float))
      (:float-to-double
       (set/check 'double-float 'single-float))

      (:double-to-int
       (set/check '(signed-byte 32) 'double-float))
      (:double-to-long
       (set/check '(signed-byte 64) 'double-float))
      (:double-to-float
       (set/check 'single-float 'double-float))


      ((:add-int :sub-int :mul-int :div-int :rem-int :and-int :or-int :xor-int :shl-int :shr-int :ushr-int)
       (set/check '(signed-byte 32) '(signed-byte 32) '(signed-byte 32)))
      ((:add-int/2addr
        :sub-int/2addr :mul-int/2addr :div-int/2addr :rem-int/2addr
        :and-int/2addr :or-int/2addr :xor-int/2addr :shl-int/2addr
        :shr-int/2addr :ushr-int/2addr)
       (check '(signed-byte 32) '(signed-byte 32)))

      ((:add-long :sub-long :mul-long :div-long :rem-long :and-long :or-long :xor-long)
       (set/check '(signed-byte 64) '(signed-byte 64) '(signed-byte 64)))
      ((:add-long/2addr :sub-long/2addr :mul-long/2addr :div-long/2addr
        :rem-long/2addr :and-long/2addr :or-long/2addr :xor-long/2addr)
       (check '(signed-byte 64) '(signed-byte 64)))
      ((:shl-long :shr-long :ushr-long)
       (set/check '(signed-byte 64) '(signed-byte 32)))
      ((:shl-long/2addr :shr-long/2addr :ushr-long/2addr)
       (check '(signed-byte 64) '(signed-byte 32)))
      ((:add-float :sub-float :mul-float :div-float :rem-float)
       (set/check 'single-float 'single-float 'single-float))
      ((:add-float/2addr :sub-float/2addr
        :mul-float/2addr :div-float/2addr :rem-float/2addr)
       (check 'single-float 'single-float))
      ((:add-double :sub-double :mul-double :div-double :rem-double)
       (set/check 'double-float 'double-float 'double-float))
      ((:add-double/2addr :sub-double/2addr
        :mul-double/2addr :div-double/2addr :rem-double/2addr)
       (check 'double-float 'double-float))

      ((:add-int/lit16 :rsub-int/lit16 :mul-int/lit16 :div-int/lit16
        :rem-int/lit16 :and-int/lit16 :or-int/lit16 :xor-int/lit16
        :add-int/lit8 :rsub-int/lit8 :mul-int/lit8 :div-int/lit8
        :rem-int/lit8 :and-int/lit8 :or-int/lit8 :xor-int/lit8
        :shr-int/lit8 :shl-int/lit8 :ushr-int/lit8)
       (set/check '(signed-byte 32) '(signed-byte 32)))
      ;; todo: figure out if we can do iget/iput here? currently
      ;; handled manually
      #++
      ((:iget :iget-wide :iget-object :iget-boolean :iget-byte :iget-char :iget-short))
      #++
      ((:iput :iput-wide :iput-object :iput-boolean :iput-byte :iput-char :iput-short))
      #++
      ((:sget :sget-wide :sget-object :sget-boolean :sget-byte :sget-char :sget-short))
      #++
      ((:sput :sput-wide :sput-object :sput-boolean :sput-byte :sput-char :sput-short))

      ;; todo: array stuff
      ((:invoke-virtual :invoke-static :invoke-super :invoke-direct
        :invoke-interface)
       (let ((o (length args)))
         (when (> o *outs*)
           (format t "~&*outs -> ~s~%" o)
           (setf *outs* o))))))
  (format t "~&assembled (~s~{ ~s~})~%" opcode args)

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
         (old (get-variable-type input)))
    (cond
      ((and old (not (eql old type)))
       (asm :check-cast input
            (java-type-string-for-class type))
       (set-variable-type input type))
      ((not old)
       (set-variable-type input type))
      ((eql type old) ;; do nothing
       )
      (t
       (break "todo? ~s ~s" old type)))

    (hir-to-dalvik (car (cleavir-ir:successors ir)))))

(defmethod hir-to-dalvik ((ir cleavir-ir:the-values-instruction))
  (let* ((input (car (cleavir-ir:inputs ir)))
         #++(.type (cleavir-ir:value-type ir))
         #++(type (clean-type .type))
         (old (get-variable-type input)))
    (format t " the-values ~s ~s?~%" input old)

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
     (let ((native-class
             (if (symbolp class)
                 (gethash class (native-classes *compile-env*))
                 class)))
       (unless native-class
         ;; print symbol in KEYWORD package to make sure we see full
         ;; package in error, since that is possible cause
         (let ((*package* (find-package :keyword)))
           (error (format nil "couldn't find class ~s?" class)
                  *variable-type-info*
                  *variable-usage*)))
       (format nil "L~a;" (java-name native-class))))))

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
     (let ((c (get-variable-type var)))
       (when (and (not c)
                  (= 1 (length (cleavir-ir:defining-instructions var)))
                  (constant-assignment-p
                   (car (cleavir-ir:defining-instructions var))))
         (setf c (constant-assignment-p
                  (car (cleavir-ir:defining-instructions var)))))
       c))
    (cons
     var)
    (symbol
     var)))

(defun java-type-string-for-variable (var)
  (let ((type (get-type-for-variable var)))
    (unless (and type (symbolp type))
      (format t "couldn't find type for ~a?~%" var))
    (java-type-string-for-class type)))

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
  (let ((arg-types (split-arg-types signature)))
    (when (= (length args) (length arg-types))
      (loop for a in args
            for vt = (get-type-for-variable a)
            for at in arg-types
            collect (if (string= (java-type-string-for-variable a) at)
                        (java-type-string-for-variable a)
                        (when (or (symbolp vt)
                                  (typep vt 'native-class))
                          (loop for i in (cpl-cache vt)
                                for ji = (java-type-string-for-class i)
                                when (string= ji at)
                                  return ji)))
              into ret
            finally (unless (position nil ret)
                      (return (list signature ret)))))))

(defun select-method-signature (method class args)
  (let* ((signatures (signatures-for-class class method))
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
    (unless (= 1 (length match))
      (format t "couldn't find signature match for ~s? ~s ~s~%"
              (cleavir-env:name method)
              nil
              (when signatures
                (alexandria:hash-table-keys signatures))))
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
    sym))

(defmethod hir-to-dalvik ((ir native-call-instruction))
  (let* ((call-type (native-call-type ir))
         (this (first (cleavir-ir:inputs ir)))
         (args (rest (cleavir-ir:inputs ir)))
         (class-name (get-variable-type this))
         (class (gethash class-name (native-classes *compile-env*)))
         (super-name (when class (extends class)))
         (super (when super-name
                  (gethash super-name (native-classes *compile-env*))))
         (name (if (typep (native-call-name ir) 'cst:cst)
                   (cst:raw (native-call-name ir))
                   (native-call-name ir)))
         (method (gethash name (native-methods *compile-env*)))
         (sig nil))
    (use-variables :native-call (cleavir-ir:inputs ir) :indexed nil)
    (ecase call-type
      (:invoke-direct
       (error ":invoke-direct todo")
       #++(asm invoke-direct (extends class)))
      (:invoke-super
       (unless class
         (error "~@<class ~s not found in while compiling :INVOKE-SUPER call to ~s? (probably in CALL-NEXT-METHOD)~:@>" class-name
                name))

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
         #++
         (when static
           (break "~s ~s ~s ~s~%" s1 s2 class-name static))
         (let ((access (third
                        (gethash s2 (gethash class-name (signatures method))))))
           (apply #'asm
                  (if static
                      :invoke-static
                      ;; special case constructor
                      (if (eq name 'java/lang/object:<init>)
                          :invoke-direct
                          :invoke-virtual))
                  (list* (java-type-string-for-class class)
                         sig)
                  this
                  args)))))
    (when (and (cleavir-ir:outputs ir)
               sig
               (cleavir-ir:using-instructions (first (cleavir-ir:outputs ir))))
      (let ((r (second (first sig)))
            (out (first (cleavir-ir:outputs ir))))
        (ecase (char r 0)
          (#\V ;; void ret, do nothing
           )
          (#\L ;; object return
           (asm :move-result-object out)
           ;; and set type so we can do casts if needed before using
           (when (get-variable-type out)
             (break "return value: type info = ~s -> ~s~%"
                    r (get-variable-type out)))
           (set-variable-type out (find-native-class r)))
          (#\I ;; int return
           (asm :move-result (first (cleavir-ir:outputs ir)))))))

    (hir-to-dalvik (car (cleavir-ir:successors ir)))))

(defmethod hir-to-dalvik ((ir cleavir-ir:assignment-instruction))
  (let ((in (car (cleavir-ir:inputs ir)))
        (out (car (cleavir-ir:outputs ir))))
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
         (set-variable-type out
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

(defun make-move-op (dest src vt)
  (etypecase vt
    ((and symbol (not null))
     (list :move-object dest src))))

(defmethod hir-to-dalvik ((ir cleavir-ir:fixed-to-multiple-instruction))
  (when (and (cleavir-ir:inputs ir)
             (cleavir-ir:outputs ir))
    #++(break "ftm")
    (use-variables :f-t-m (list (car (cleavir-ir:inputs ir))))
    (use-variables :f-t-m (list (car (cleavir-ir:outputs ir))))
    (let ((vt (get-variable-type (car (cleavir-ir:inputs ir)))))
      (assert vt)
      (set-variable-type (car (cleavir-ir:outputs ir)) vt)
      (apply #'asm (make-move-op (car (cleavir-ir:outputs ir))
                                 (car (cleavir-ir:inputs ir))
                                 vt))))
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:multiple-to-fixed-instruction))
  #++
  (when (cleavir-ir:outputs ir)
    (break "todo: multiple-to-fixed-instruction not implemented yet"))
  (when (and (cleavir-ir:inputs ir)
             (cleavir-ir:outputs ir))
    #++(break "mtf")
    (use-variables :m-t-f (list (car (cleavir-ir:inputs ir))))
    (use-variables :m-t-f (list (car (cleavir-ir:outputs ir))))
    (let ((vt (get-variable-type (car (cleavir-ir:inputs ir)))))
      (assert vt)
      (set-variable-type (car (cleavir-ir:outputs ir)) vt)
      (apply #'asm (make-move-op (car (cleavir-ir:outputs ir))
                                 (car (cleavir-ir:inputs ir))
                                 vt))))
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:nop-instruction))
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir add-instruction))
  (when (cleavir-ir:outputs ir)
    (assert (= 2 (length (cleavir-ir:inputs ir))))
    (use-variables :add (cleavir-ir:inputs ir))
    (asm :add-int (first (cleavir-ir:outputs ir))
         (first (cleavir-ir:inputs ir))
         (second (cleavir-ir:inputs ir))))
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:slot-write-instruction))
  (let* ((object (first (cleavir-ir:inputs ir)))
         (value (second (cleavir-ir:inputs ir)))
         (class (gethash (get-variable-type object)
                         (native-classes *3bil2-environment*)))
         (slot (find (slot-name ir) (fields class) :key 'name))
         (slot-id (list (java-type-string-for-class class)
                        (field-name slot)
                        (field-type slot))))
    (assert slot)
    (use-variables :write-slot (list value object))
    (ecase (char (field-type slot) 0) ;; todo: other types
      (#\I
       (asm :iput value object slot-id))
      (#\L
       (asm :iput-object value object slot-id))))
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:slot-read-instruction))
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
    (ecase (char (field-type slot) 0) ;; todo: other types
      (#\I
       (asm :iget out object slot-id))
      (#\L
       (asm :iget-object out object slot-id))))
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defmethod hir-to-dalvik ((ir cleavir-ir:return-instruction))
  (let* ((v (car (cleavir-ir:inputs ir)))
         (d (cleavir-ir:defining-instructions v)))
    (if (and (= 1 (length d))
             (typep (car d) 'cleavir-ir:fixed-to-multiple-instruction)
             (zerop (length (cleavir-ir:inputs (car d)))))
        (progn
          (assert (or (not *return-type*)
                      (eql *return-type* :void)))
          (setf *return-type* :void)
          (asm :return-void))
        (let ((type (get-variable-type v)))
          (unless type
            (break "~s" v
                   *variable-type-info*
                   *variable-usage*))
          (assert (or (not *return-type*)
                      (eql *return-type* type)))
          (setf *return-type* type)
          (use-variables :return (cleavir-ir:inputs ir) :indexed nil)
          (asm :return v)))))

(defmethod hir-to-dalvik ((ir asm-instruction))
  (use-variables :return (cleavir-ir:inputs ir) :indexed nil)
  (loop for i in (code ir) do (apply 'asm i))
  (hir-to-dalvik (car (cleavir-ir:successors ir))))

(defvar *moves*
  (alexandria:plist-hash-table
   '(:move (:move/from16 :move/16)
     :move-wide (:move-wide/from16 :move-wide/16)
     :move-object (:move-object/from16 :move-object/16))))
(defun use-sized-op (asm)
  (print
   (let ((op (first asm)))
     (cond
       ;; use a larger move op if needed
       ((gethash op *moves*)
        (let ((d (second asm))
              (s (third asm))
              (m (gethash op *moves*)))
          (cond
            ((> d 255) ;; use /16 if dest is more than 8 bits
             (list* (second m) (cdr asm)))
            ((> s 15) ;; use /from16 if source is more than 4 bits
             (list* (first m) (cdr asm)))
            (asm))))
       ;; just return it
       (asm)))))

(defun use-regs (alloc code)
  (mapcar 'use-sized-op
          (loop for inst in code
                for op = (car inst)
                for args = nil
                for moves = nil
                when (member op '(:invoke-virtual :invoke-static
                                  :invoke-super :invoke-direct
                                  :invoke-interface))
                  do (assert (<= (length (cdr inst)) *outs*))
                     (loop for i in (cdr inst)
                           for n from 0
                           for a = (gethash i alloc)
                           when (and a (>= a 16))
                             do (format t "making move: ~s <- ~s @ ~s (~s)~%"
                                        n a i (get-variable-type i))
                                (push (make-move-op n a (get-variable-type i))
                                      moves)
                                (push n args)
                           else when a
                                  do (push a args)
                           else do (push i args))
                  and append (nreverse moves)
                  and collect (list* op (nreverse args))
                else
                  collect (loop for i in inst
                                when (gethash i alloc)
                                  collect it
                                else collect i))))

(defun regalloc (uses code)
  (let ((alloc (make-hash-table))
        (counter -1)
        (max 0)
        (args))
    (when (> (hash-table-count uses) 15)
      ;; if we are using more than 15 variables, allocate some
      ;; registers specifically for function call arguments (probably
      ;; should sort registers by usage type and do multiple passes
      ;; and see if any of the high registerss get passed to ops that
      ;; can't accept them first)
      (setf counter (min 5 *outs*)))
    (loop for v being the hash-keys of uses
            using (hash-value u)
          for e = (assoc :enter u)
          when e
            do (push (list v (second e)) args)
          else do (setf (gethash v alloc)
                        (or (gethash v alloc) (incf counter))))
    (incf counter)
    (loop for (v i) in args
          do (assert (not (gethash v alloc)))
             (setf max (max max (+ i counter)))
             (setf (gethash v alloc) (+ i counter)))
    (format t "alloc->~{~%  ~a ~a~}~%"
            (alexandria:alist-plist
             (sort (alexandria:hash-table-alist alloc)
                   '< :key 'cdr)))
    (values
     (use-regs alloc code)
     (loop for (i) in (sort (copy-list args) '< :key 'second)
           collect (java-type-string-for-variable i))
     (1+ max)
     (format nil "(~{~a~})~a"
             ;; don't include 'this' in signature
             (loop for (a) in (cdr (sort (copy-list args)
                                         '< :key 'second))
                   collect (java-type-string-for-variable a))
             (java-type-string-for-class *return-type*))
     (java-type-string-for-class *return-type*)
     *outs*)))

(defun compile-hir (hir env &optional arg-types)
  (format t "~&compile-hir~s~%" hir)
  (let ((*variable-type-info* (make-hash-table))
        (*current-code* nil)
        (*compile-env* env)
        (*variable-usage* (make-hash-table))
        (*return-type* nil)
        (*outs* 0))
    (when arg-types
      (loop for i in (cleavir-ir:lambda-list hir)
            for a in arg-types
            when a
              do (set-variable-type i a)))
    (hir-to-dalvik hir)
    (setf *current-code* (reverse *current-code*))
    (format t "~%~%compiled ~s~%" *current-code*)
    (format t "~%outs: ~s~%" *outs*)
    (format t "regalloc: ~s~%~{  ~a ~a~%~}~%"
            (hash-table-count *variable-usage*)
            (alexandria:hash-table-plist *variable-usage*))
    (regalloc *variable-usage* *current-code*)))
