;;; -*- Lisp -*-

(defsystem "utilities"
  :depends-on ("series")
  :components ((:file "package")
               (:file "macros" :depends-on ("package"))
               (:file "utilities" :depends-on ("macros" "package"))))

