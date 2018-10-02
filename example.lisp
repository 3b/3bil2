#++
(asdf:load-system '3bil2)
(in-package 3bil2)

(compile-toplevel
 '(progn
   (defclass-native hello-activity (android/app:activity)
    ())
   (defmethod-native java/lang/object:<init> ((this hello-activity))
    (declare (values))
    #++(call-next-method)
    (java/lang/object:<init> (super this))
    (values))

   (defmethod-native android/app/activity:on-create ((this hello-activity)
                                                     (saved-instance-state
                                                      android/os:bundle))
    (declare (values))
    #++(call-next-method)
    (android/app/activity:on-create (super this)
     saved-instance-state)
    (android/app/activity:set-content-view this 2130903040)
    (values))))
