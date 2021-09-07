;;; -*- Lisp -*-

(in-package "UTILITIES")

(defmacro named-lambda ((name &rest arglist) &body body)
  `(labels ((,name ,arglist ,@body))
     #',name))

(defmacro let (name-or-bindings bindings-or-first &body body)
  (cond ((symbolp name-or-bindings)
         `(funcall (named-lambda (,name-or-bindings ,@(map 'list #'car bindings-or-first))
                                 ,@body)
                   ,@(map 'list (lambda (binding)
                                  `(progn ,@(cdr binding))) bindings-or-first)))
        ((or (null name-or-bindings)
             (consp name-or-bindings))
         `(series::let ,name-or-bindings
            ,bindings-or-first
            ,@body))
        (t (error "Bad syntax: LET"))))
         
