(in-package 3bil2)

;; helper for generating a defpackage form for a java package
;; (intended to be used once to generate the form which is then edited
;; manually for exports etc, but will try to add exports as needed, so
;; can be run again to generate full package after defining methods)
(defun generate-package-for-subclass (class-name
                                      &key (extends 'java/lang:object)
                                        implements
                                        (copy-existing t)
                                        (ids-file))
  (flet ((class-package-name (s)
           (format nil "~a/~a" (package-name (symbol-package s)) (string s)))
         (public-method-p (sigs)
           (loop for s in (alexandria:hash-table-values sigs)
                 count (member :protected
                               (third s))
                   into protected
                 count (member :public
                               (third s))
                   into public
                 finally (return
                           (list
                            (plusp protected)
                            (plusp public))))))
    (let* ((classes (native-classes *3bil2-environment*))
           (class (gethash class-name classes))
           (superclass (gethash extends classes))
           (interfaces (loop for i in implements
                             collect (gethash i classes)))
           (containing-package (symbol-package class-name))
           (r (format nil "~a/R" (package-name (symbol-package class-name)))))
      `(progn
         (defpackage ,r
           (:use)
           (:export ,@(when (find-package r)
                        (let ((a nil))
                          (do-external-symbols (x (find-package r))
                            (push (symbol-name x) a))
                          (sort a 'string<)))))
         (defpackage ,(format nil "~a/~a"
                              (package-name (symbol-package class-name))
                              (string class-name))
           (:use :cl ,(class-package-name extends)
                 ,@ (mapcar #'class-package-name
                            implements))
           (:import-from :3bil2
            :defclass-native :defmethod-native :new)
           (:local-nicknames ("R" ,r))
           ,@(when containing-package
               `((,(if (loop for c in (cons extends implements)
                             for p = (find-package (class-package-name c))
                               thereis
                               (eql :external
                                    (nth 1 (find-symbol (string
                                                         class-name)
                                                        p))))
                       :shadowing-import-from
                       :import-from)
                  ,(package-name containing-package)
                  ,(string class-name))))
           ,@(loop with imports = (make-hash-table)
                   with shadow = (make-hash-table)
                   for cn in (cons extends implements)
                   for c in (cons superclass interfaces)
                   for p = (or (find-package cn)
                               (find-package (class-package-name cn)))
                   do (labels ((imp* (s h)
                                 (let ((p (symbol-package s)))
                                   (unless (gethash p h)
                                     (setf (gethash p h)
                                           (make-hash-table)))
                                   (setf (gethash s (gethash p h)) s)))
                               (imp (s pub prot)
                                 (if (and pub
                                          (find-symbol (symbol-name s)
                                                       (find-package :cl)))
                                     (imp* s shadow)
                                     (when prot(imp* s imports)))))
                        (loop for f in (fields c)
                              for pub = (member :public (access f))
                              for prot = (member :protected (access f))
                              do (imp (name f) pub prot))
                        (loop for m in (public-methods c :include-protected t)
                              for (prot pub) = (public-method-p
                                                (signatures-for-class cn m))
                              do (imp (cleavir-env:name m) pub prot)))
                   finally
                      (return
                        (append
                         (loop for p in (sort (alexandria:hash-table-keys shadow)
                                              'string< :key 'package-name)
                               for s = (sort (alexandria:hash-table-keys
                                              (gethash p shadow))
                                             'string< :key 'string)
                               collect `(:shadowing-import-from ,(package-name p)
                                                                ,@ (mapcar 'string s)))
                         (loop for p in (sort (alexandria:hash-table-keys imports)
                                              'string< :key 'package-name)
                               for s = (sort (alexandria:hash-table-keys
                                              (gethash p imports))
                                             'string< :key 'string)
                               collect `(:import-from ,(package-name p)
                                                      ,@ (mapcar 'string s))))))
           (:export
            ,@(when (and copy-existing class)
                (let ((s (make-hash-table)))
                  (loop for m in (public-methods class)
                        do (setf (gethash (cleavir-env:name m) s) t))
                  (loop for f in (coerce (fields class) 'list)
                        when (member :public (access f))
                          do (setf (gethash (name f) s) t))
                  (sort (mapcar 'string (alexandria:hash-table-keys s))
                        'string<)))))))))
