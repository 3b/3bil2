(defsystem :3bil2
  :description "CL-like language for android dalvik/ART runtime"
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on ("3b-dex"
               "sicl-simple-environment"
               "cleavir-generate-ast"
               "cleavir-ir"
               "cleavir-compilation-policy"
               "cleavir-ast-to-hir"
               "cleavir-ast-transformations"
               "cleavir-kildall-type-inference"
               "cleavir-escape"
               "cleavir-hir-transformations"
               "cleavir-remove-useless-instructions"
               "cleavir-hir-to-mir"
               "cleavir-basic-blocks")
  :serial t
  :components ((:file "package")
               (:file "system")
               (:file "env")
               (:file "ir")
               (:file "codegen")))
