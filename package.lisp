(defpackage :3bil2
  (:use :cl)
  (:export
   :defmethod-native
   :defclass-native
   :new
   ))

(defpackage :3bil2-lir
  (:use)
  (:export

   ))

(defpackage :3bil2-cl-impl
  ;; things that overwrite CL symbols
  (:export

   ))
