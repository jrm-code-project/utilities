;;; -*- Lisp -*-

(defsystem "stream"
  :depends-on ("promise" "utilities")
  :components ((:file "package")
               (:file "stream" :depends-on ("package"))))
