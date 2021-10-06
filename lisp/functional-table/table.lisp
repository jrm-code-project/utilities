;;; -*- Lisp -*-

(in-package "FUNCTIONAL-TABLE")

(defun append (primary secondary)
  (lambda (query if-found if-not-found)
    (funcall primary query
             if-found
             (lambda ()
               (funcall secondary query if-found if-not-found)))))

(defun empty ()
  (lambda (query if-found if-not-found)
    (declare (ignore query if-found))
    (funcall if-not-found)))

(defun extend (table key value)
  (lambda (query if-found if-not-found)
    (if (or (eql query key)
            (equal (symbol-name query) (symbol-name key)))
        (funcall if-found value)
        (funcall table query if-found if-not-found))))

(defun extend* (table keys values)
  (fold-left #'extend table keys values))

(defun lookup (table query if-found if-not-found)
  (funcall table query if-found if-not-found))

(defun redact (table key)
  (lambda (query if-found if-not-found)
    (if (eql query key)
        (funcall if-not-found)
        (funcall table query if-found if-not-found))))

(defun redact* (table keys)
  (fold-left #'redact table keys))
