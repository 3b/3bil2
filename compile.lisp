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

(defun compile-toplevel-1 (form &key env)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
         (sys (make-instance '3bil2))
         (env (or env *3bil2-environment*))
         (cst (cst:cst-from-expression form))
         (ast (cleavir-cst-to-ast:cst-to-ast cst env sys))
         (hir (unless (empty-form-p ast)
                (cleavir-ast-to-hir:compile-toplevel-unhoisted ast))))
    (when hir
      (cleavir-kildall-type-inference:infer-types hir env :prune t)
      (cleavir-hir-transformations:eliminate-typeq hir)
      (cleavir-hir-transformations:eliminate-superfluous-temporaries hir)

      (list (multiple-value-list
             (compile-hir hir env))
            hir ast))))

(defun compile-toplevel (form &key env)
  (if (typep form '(cons (eql progn)))
      (remove 'nil (mapcar (lambda (a) (compile-toplevel-1 a :env env))
                           (cdr form)))
      (compile-toplevel-1 form :env env)))
