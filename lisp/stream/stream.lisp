;;; -*- Lisp -*-

;; CL:STREAM is shadowed
(in-package "STREAM")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass stream ()
  ((car :initarg :car
        :initform (error "Required initarg :car omitted.")
        :reader stream-car)
   (delayed-cdr :initarg :delayed-cdr
                :initform (error "Required initarg :delayed-cdr omitted.")
                :reader stream-delayed-cdr))))

(defmacro cons-stream (car cdr)
  `(make-instance 'stream :car ,car :delayed-cdr (delay ,cdr)))

(defmacro rec-stream (name value)
  `(let ((,name nil))
     (setq ,name ,value)
     ,name))

(defun stream-cdr (stream)
  (check-type stream stream)
  (force (stream-delayed-cdr stream)))

(defconstant the-empty-stream '())

(defun empty-stream? (thing)
  (null thing))

(defun singleton-stream (element)
  (cons-stream element the-empty-stream))

(defun infinite-stream (element)
  (rec-stream stream (cons-stream element stream)))

(defun scan-stream (stream)
  (declare (optimizable-series-function))
  (map-fn t #'stream-car (scan-fn 'stream (lambda () stream) #'stream-cdr #'empty-stream?)))

(defun show-stream (stream &optional (length 5))
  (do ((stream stream (stream-cdr stream))
       (count 0 (+ count 1)))
      ((or (empty-stream? stream) (>= count length))
       (format t " ~:[...~;~]}" (empty-stream? stream)))
    (format t "~:[ ~;{~]~s~%" (zerop count) (stream-car stream))))

(defun list->stream (list)
  (fold-right (lambda (element tail)
                (cons-stream element tail))
              list
              the-empty-stream))

(defun stream (&rest elements)
  (list->stream elements))

(defun stream-fold-right (f stream final)
  (if (empty-stream? stream)
      final
      (funcall f
               (stream-car stream)
               (stream-fold-right f (stream-cdr stream) final))))

(defun stream-fold-right-delayed (f stream final)
  (if (empty-stream? stream)
      final
      (funcall f
               (stream-car stream)
               (delay (stream-fold-right-delayed f (stream-cdr stream) final)))))

(defun stream-append2-delayed (left delayed-right)
  (if (empty-stream? left)
      (force delayed-right)
      (cons-stream (stream-car left)
                   (stream-append2-delayed (stream-cdr left) delayed-right))))

(defun stream-interleave2-delayed (left delayed-right)
  (if (empty-stream? left)
      (force delayed-right)
      (cons-stream (stream-car left)
                   (stream-interleave2-delayed (force delayed-right)
                                               (stream-delayed-cdr left)))))

(defun stream-flatten-append (stream-of-streams)
  (stream-fold-right-delayed #'stream-append2-delayed stream-of-streams the-empty-stream))

(defun stream-flatten-interleave (stream-of-streams)
  (stream-fold-right-delayed #'stream-interleave2-delayed stream-of-streams the-empty-stream))

(defun stream-generate (f value)
  (cons-stream value (stream-generate f (funcall f value))))

(defun stream-limit (stream tolerance &optional (n-terms most-positive-fixnum))
  (flet ((close-enough? (h1 h2)
           (<= (abs (- h1 h2))
               (* .5
                  tolerance
                  (+ (abs h1) (abs h2) 2)))))
    (labels ((l (s count)
               (let* ((h0 (stream-car s))
                      (tail (stream-cdr s))
                      (h1 (stream-car tail)))
                 (if (or (close-enough? h0 h1)
                         (>= count n-terms))
                     h1
                     (l tail (+ count 1))))))
      (l stream 0))))

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
                   ((find-if (lambda (stream)
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

(defun stream-ref (stream n)
  (check-type n (integer 0 *))
  (labels ((l (stream n)
             (cond ((empty-stream? stream) (error "Stream too short in STREAM-REF."))
                   ((zerop n) (stream-car stream))
                   (t (l (stream-cdr stream) (- n 1))))))
    (l stream n)))

(defun stream-scale (factor stream)
  (stream-map (lambda (element) (* element factor)) stream))

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

(defun power-stream (base)
  (rec-stream stream (cons-stream 1 (stream-scale base stream))))

(defun ones ()
  (rec-stream ones (cons-stream 1 ones)))

(defun naturals ()
  (rec-stream naturals (cons-stream 1 (stream-map #'+ (ones) naturals))))

(defun integers ()
  (cons-stream 0 (naturals)))

(defun squares ()
  (stream-map #'square (naturals)))

(defun evens ()
  (stream-scale 2 (integers)))

(defun odds ()
  (stream-map #'+ (evens) (ones)))

(defun pi/4-series ()
  (stream-map #'/ (power-stream -1) (odds)))

(defun partial-sums (series)
  (stream-accumulate #'+ 0 series))

(defun euler-transform (series)
  (cons-stream (/ (stream-car series) 2)
               (euler-transform (stream-map #'average series (stream-cdr series)))))

(defun weighted-average (weight left right)
  (+ (* (- 1 weight) left)
     (* weight right)))

(defun weighted-euler-transform (weights series)
  (cons-stream (* (stream-car series) (stream-car weights))
               (weighted-euler-transform
                (stream-cdr weights)
                (stream-map #'weighted-average weights series (stream-cdr series)))))

(defun archimedes-pi-sequence ()
  (flet ((refine-by-doubling (s)
           (/ s (sqrt (+ 2 (sqrt (- 4 (* s s)))))))

         (semi-perimeter (side-length side-number)
           (* (/ side-number 2) side-length)))

    (let ((side-lengths (stream-generate #'refine-by-doubling (sqrt 2.0d0)))
          (side-numbers (stream-generate (lambda (n) (* 2 n)) 4)))

      (stream-map #'semi-perimeter side-lengths side-numbers))))

;;; Richardson extrapolation

(defun make-zeno-sequence (f h)
  (cons-stream (funcall f h) (make-zeno-sequence f (/ h 2))))

(defun accelerate-zeno-sequence (sequence order)
  (let* ((2^p   (expt 2 order))
         (2^p-1 (- 2^p 1)))
    (stream-map (lambda (rh rh/2)
                  (/ (- (* 2^p rh/2) rh)
                     2^p-1))
                sequence
                (stream-cdr sequence))))

(defun make-zeno-tableau (sequence order increment)
  (labels ((sequences (sequence order)
             (cons-stream sequence
                          (sequences (accelerate-zeno-sequence sequence order)
                                     (+ order increment)))))
    (sequences sequence order)))

(defun first-term-of-zeno-tableau (tableau)
  (stream-map #'stream-car tableau))

(defun richardson-sequence (sequence order increment)
  (first-term-of-zeno-tableau (make-zeno-tableau sequence order increment)))

(defun richardson-limit (f start-h ord inc tolerance &optional (n-terms most-positive-fixnum))
  (stream-limit
   (richardson-sequence (make-zeno-sequence f start-h)
                        ord
                        inc)
   tolerance
   n-terms))

(defun make-derivative (f)
  (let ((h (sqrt double-float-epsilon)))
    (lambda (x)
      (/ (- (funcall f (+ x h)) (funcall f (- x h)))
         2 h))))

(defun diff-quot-stream (f x h)
  (cons-stream (/ (- (funcall f (+ x h)) (funcall f (- x h))) 2 h)
               (diff-quot-stream f x (/ h 2))))

(defun rderiv (f tolerance)
  (lambda (x)
    (let* ((h (/ (abs x) 2))
           (delta (- (funcall f (+ x h)) (funcall f (- x h))))
           (roundoff (* double-float-epsilon
                        (+ 1 (floor (abs (/ (funcall f x) delta))))))
           (n (floor (/ (log (/ tolerance roundoff))
                        (log 2)))))
      (richardson-limit (lambda (dx) (/ (- (funcall f (+ x dx))
                                           (funcall f (- x dx)))
                                        2 dx))
                        h
                        2
                        2
                        tolerance
                        (+ n 1)))))

(defun trapezoid-sums (f a b)
  (flet ((next-s (s n)
           (let* ((h (/ (- b a) 2 n))
                  (fx (lambda (i) (funcall f (+ a (* (+ i i -1) h))))))
             (+ (/ s 2) (* h (big-sigma fx 1 (+ n 1)))))))
    (labels ((s-and-n-stream (s n)
               (cons-stream (list s n)
                            (s-and-n-stream (next-s s n) (* n 2)))))
      (let* ((h (- b a))
             (s (* (/ h 2) (+ (funcall f a) (funcall f b)))))
        (stream-map #'car (s-and-n-stream s 1))))))

(defun romberg-stream (f a b)
  (richardson-sequence (trapezoid-sums f a b) 2 2))

(defun romberg (f a b tolerance)
  (stream-limit (romberg-stream f a b) tolerance))

;(defun pi-estimator (n)
;  (funcall (trapezoid (lambda (x) (/ 4 (+ 1 (* x x)))) 0 1) n))

