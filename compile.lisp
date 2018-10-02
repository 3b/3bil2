(in-package 3bil2)

(defun collect-inherited-methods (class super-class)
  (loop for m in (methods
                  (gethash super-class (native-classes *3bil2-environment*)))
        for sigs = (loop for (sc s a ?) in (signatures m)
                         when (and (eq sc super-class)
                                   (intersection a '(:public :protected)))
                           count it
                           and do (pushnew (list class s a ?)
                                           (signatures m)
                                           :test 'equalp))
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
