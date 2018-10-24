(defpackage 3bil2/asdf
  (:use :cl :asdf))
(in-package 3bil2/asdf)

(defclass 3bil2-file (cl-source-file)
  ())

(defmethod perform ((op compile-op) (r 3bil2-file))
  ;; indirect call to avoid needing to load 3bil2 to compile this
  (funcall (find-symbol "3BIL2-COMPILE-FILE" (find-package "3BIL2"))
           (first (input-files op r))))

(defmethod perform ((op load-op) (r 3bil2-file))
  (format t "load-op ~s~%" (component-name r))
  ;; no-op for now since everything happens at compile time
  nil)


(setf (find-class 'asdf::3bil2-file)
      (find-class '3bil2-file))

