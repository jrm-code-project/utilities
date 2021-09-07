;;; -*- Lisp -*-

(defsystem promise
  :depends-on ()
  :components ((:file "package")
               (:file "promise" :depends-on ("package"))))
