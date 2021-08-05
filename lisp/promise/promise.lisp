;;; -*- Lisp -*-

(in-package "PROMISE")

(defclass promise ()
  ((forced? :initform nil)
   (values-or-thunk :initarg :thunk
                    :initform (error "Required initarg :thunk omitted.")))
  (:documentation "A simple call-by-need thunk."))

(defmethod print-object ((object promise) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[un~]forced" (not (slot-value object 'forced?)))))

(defmacro delay (expression)
  "Delays an expression and returns a promise."
  `(make-instance 'promise :thunk (lambda () ,expression)))

(defun force (promise)
  "Returns the values of a promise, forcing it if necessary."
  (check-type promise promise)
  ;; Ensure the values have been memoized.
  (unless (slot-value promise 'forced?)
    (let ((values (multiple-value-list (funcall (slot-value promise 'values-or-thunk)))))
      ;; If this is not a recursive call, memoize the result.
      ;; If this is a recursive call, the result is discarded.
      (unless (slot-value promise 'forced?)
        (setf (slot-value promise 'values-or-thunk) values)
        (setf (slot-value promise 'forced?) t))))
  ;; Return the memoized values.
  (values-list (slot-value promise 'values-or-thunk)))

(defun forced? (promise)
  (check-type promise promise)
  (slot-value promise 'forced?))

(defun promise-values (promise if-forced if-unforced)
  "Invokes if-forced on the promise values if the promise has been forced,
otherwise invokes if-unforced."
  (check-type promise promise)
  (if (slot-value promise 'forced?)
      (apply if-forced (slot-value promise 'values-or-thunk))
      (funcall if-unforced)))
