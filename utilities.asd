;;; -*- Lisp -*-

(defsystem "utilities"
  :depends-on ("series")
  :components ((:file "package")
               (:file "utilities" :depends-on ("package"))))
