;;; -*- Lisp -*-

;; CL:STREAM is shadowed
(in-package "STREAM")

(defclass stream ()
  ((car :initarg :car
        :initform (error "Required initarg :car omitted.")
        :reader stream-car)
   (delayed-cdr :initarg :delayed-cdr
                :initform (error "Required initarg :delayed-cdr omitted.")
                :reader stream-delayed-cdr)))

(defmacro cons-stream (car cdr)
  `(make-instance 'stream :car ,car :delayed-cdr (delay ,cdr)))

(defun stream-cdr (stream)
  (check-type stream stream)
  (force (stream-delayed-cdr stream)))

(defconstant the-empty-stream '())

(defun empty-stream? (thing)
  (null thing))

(defun stream-map (function stream &rest streams)
  (labels ((stream-map-1 (stream)
             (cond ((typep stream 'stream) (cons-stream (funcall function (stream-car stream))
                                                        (stream-map-1 (stream-cdr stream))))
                   ((eq stream the-empty-stream) the-empty-stream)
                   (t (error "Unexpected thing found in stream."))))

           (stream-map-n (streams)
             (cond ((every (lambda (stream)
                             (typep stream 'stream))
                           streams)
                    (cons-stream (apply function (map 'list #'stream-car streams))
                                 (stream-map-n (map 'list #'stream-cdr streams))))
                   ((every (lambda (stream)
                             (eq stream the-empty-stream)) streams)
                    the-empty-stream)
                   (t (error "Streams are unequal or something unexpected found.")))))
    (if (consp streams)
        (stream-map-n (cons stream streams))
        (stream-map-1 stream))))

(defun stream-accumulate (combiner init stream)
  (let ((result-stream nil))
    (setq result-stream (stream-map combiner (cons-stream init result-stream) stream))
    result-stream))

(defun stream-truncate-delayed (delayed-stream n-elements)
  (if (zerop n-elements)
      the-empty-stream
      (let ((tail (force delayed-stream)))
        (cons-stream (stream-car tail)
                     (stream-truncate-delayed (stream-delayed-cdr tail) (- n-elements 1))))))

(defun stream-truncate (stream n-elements)
  (if (zerop n-elements)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (stream-truncate-delayed (stream-delayed-cdr stream) (- n-elements 1)))))

(defun double-stream (stream)
  (if (null stream)
      '()
      (cons-stream (stream-car stream)
                   (cons-stream (stream-car stream)
                                (double-stream (stream-cdr stream))))))

(defun ones ()
  (let ((ones nil))
    (setq ones (cons-stream 1 ones))
    ones))

(defun naturals ()
  (let ((naturals nil))
    (setq naturals (cons-stream 1 (stream-map #'+ (ones) naturals)))
    naturals))

(defun integers ()
  (cons-stream 0 (naturals)))

(defun squares ()
  (stream-map #'square (naturals)))

(defun evens ()
  (stream-map (lambda (x) (* x 2)) (integers)))

(defun odds ()
  (stream-map #'+ (evens) (ones)))
