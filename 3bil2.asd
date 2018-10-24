(defsystem :3bil2
  :description "CL-like language for android dalvik/ART runtime"
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on ("3b-dex"
               "3bil2/env"
               "3bil2-ffigen"
               "cleavir-cst-to-ast")
  :serial t
  :components ((:file "ir")
               (:file "codegen")
               (:file "compile")
               (:file "utils")
               (:file "macros")))

(defsystem :3bil2/env
  ;; separate since ffi defs depend on it to load ffi, and codegen etc
  ;; wants to look at symbols created by ffi
  :depends-on ("sicl-simple-environment"
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
               (:file "env")))

(defsystem :3bil2/asdf
  :depends-on ()
  :serial t
  :components ((:file "asdf")))
