;;; -*- Lisp -*-

(defsystem "functional-table"
  :depends-on ("utilities")
  :components ((:file "package")    
               (:file "table" :depends-on ("package"))))

