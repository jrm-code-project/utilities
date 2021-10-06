;;; -*- Lisp -*-

(in-package "UTILITIES")

(defun average (a b)
  (/ (+ a b) 2))

;;; Limit is exclusive.
(defun big-pi (function start limit)
  (collect-product
   (map-fn 'number function
           (scan-range :from start :below limit))))

;;; Limit is exclusive.
(defun big-sigma (function start limit)
  (collect-sum
   (map-fn 'number function
           (scan-range :from start :below limit))))

(defun binomial (n k)
  (cond ((zerop k) 1)
        ((= n k) 1)
        (t (+ (binomial (- n 1) (- k 1))
              (binomial (- n 1) k)))))

(defun cross-ratio (a b c d)
  (/ (* (- a c) (- b d))
     (* (- a d) (- b c))))

(defun cube (x)
  (* x x x))

(declaim (ftype (function ((function (t t &rest t) t) t t &rest t) t) fold-left))
(defun fold-left (function initial list &rest lists)
  (labels ((fold-left-1 (state item tail)
             (declare (optimize (debug 0) (safety 0) (speed 3)))
             (cond ((consp tail)
                    (fold-left-1 (funcall function state item) (car tail) (cdr tail)))
                   ((null tail) (funcall function state item))
                   (t (error "Dotted list encountered by fold-left."))))

           (fold-left-n (state items tails)
             (cond ((every #'consp tails)
                    (fold-left-n (apply function state items) (map 'list #'car tails) (map 'list #'cdr tails)))
                   ((every #'null tails) (apply function state items))
                   (t (error "Lists of different lengths or dotted list in fold-left.")))))

    (if (null lists)
        (cond ((consp list) (fold-left-1 initial (car list) (cdr list)))
              ((null list) initial)
              (t (error "Non list in fold-left.")))
        (let ((tails (cons list lists)))
          (cond ((every #'consp tails)
                 (fold-left-n initial (map 'list #'car tails) (map 'list #'cdr tails)))
                ((every #'null tails) initial)
                (t (error "Non list in fold-left.")))))))

(defun factorial (x)
  (big-pi #'identity 1 (+ x 1)))

(defun falling-factorial (x fall)
  (big-pi #'identity (- x (- fall 1)) (+ x 1)))

(defun fold-right (function list final &rest args)
  (labels ((fold-right-1 (tail final)
             (cond ((consp tail)
                    (funcall function (car tail) (fold-right-1 (cdr tail) final)))
                   ((null tail) final)
                   (t (error "Dotted list encountered by fold-right."))))

           (fold-right-n (tails final)
             (cond ((every #'consp tails)
                    (apply function (append (map 'list #'car tails)
                                            (list (fold-right-n (map 'list #'cdr tails) final)))))
                   ((every #'null tails)
                    final)
                   (t (error "Lists of different lengths or dotted list in fold-right.")))))
    (if (null args)
        (fold-right-1 list final)
        (fold-right-n (list* list final (butlast args)) (car (last args))))))

(defun harmonic (n)
  (big-sigma (lambda (n) (/ 1 n)) 1 (+ n 2)))

(defun integer-log (n base)
  "Returns two values, the integer-log of <n> in <base>, and the leftmost digit
in <base>."
  (if (< n base)
      (values 0 n)
      (multiple-value-bind (ilog l) (integer-log n (* base base))
        (if (< l base)
            (values (* ilog 2) l)
            (values (+ (* ilog 2) 1) (/ l base))))))

(defun rising-factorial (x rise)
  (big-pi #'identity x (+ x rise)))

(defun square (number)
  "Square a number by multiplying it by itself."
  (* number number))

(defun least-squares (points)
  (do ((points points (cdr points))
       (sigma-x 0 (+ sigma-x (caar points)))
       (sigma-y 0 (+ sigma-y (cdar points)))
       (sigma-x-squared 0 (+ sigma-x-squared (square (caar points))))
       (sigma-xy 0 (+ sigma-xy (* (caar points) (cdar points))))
       (count 0 (+ count 1)))
      ((null points)
       (let* ((m (/ (- (* count sigma-xy) (* sigma-x sigma-y))
                    (- (* count sigma-x-squared) (* sigma-x sigma-x))))
              (b (/ (- sigma-y (* m sigma-x)) count)))
         (values m b)))))

(defun leftmost-digit (n base)
  (if (< n base)
      n
      (let ((l (leftmost-digit n (* base base))))
        (if (< l base)
            l
            (truncate l base)))))

(defun list-length-compare (left right left-null right-null both-null name)
  (cond ((consp left)
         (cond ((consp right)
                (list-length-compare (cdr left) (cdr right) left-null right-null both-null name))
               ((null right) right-null)
               (t (error (format nil "Dotted list encountered in ~s" name)))))
        ((null left)
         (cond ((consp right) left-null)
               ((null right) both-null)
               (t (error (format nil "Dotted list encountered in ~s" name)))))
        (t (error (format nil "Dotted list encountered in ~s" name)))))

(defun list-length<? (left right)
  (list-length-compare left right t nil nil 'list-length<?))

(defun list-length<=? (left right)
  (list-length-compare left right t nil t 'list-length<=?))

(defun list-length=? (left right)
  (list-length-compare left right nil nil t 'list-length=?))

(defun list-length<>? (left right)
  (list-length-compare left right t t nil 'list-length<>?))

(defun list-length>=? (left right)
  (list-length-compare left right nil t t 'list-length>=?))

(defun list-length>? (left right)
  (list-length-compare left right nil t nil 'list-length>?))

(defun list-length-compare-int (list n left-null right-zero both-end name)
  (cond ((consp list)
         (cond ((plusp n)
                (list-length-compare-int (cdr list) (- n 1) left-null right-zero both-end name))
               ((zerop n) right-zero)
               (t (error (format nil "Negative n encountered in ~s" name)))))
        ((null list)
         (cond ((plusp n) left-null)
               ((zerop n) both-end)
               (t (error (format nil "Negative n encountered in ~s" name)))))
        (t (error (format nil "Dotted list encountered in ~s" name)))))

(defun list-length<int? (list n)
  (list-length-compare-int list n t nil nil 'list-length<int?))

(defun list-length<=int? (list n)
  (list-length-compare-int list n t nil t 'list-length<=int?))

(defun list-length=int? (list n)
  (list-length-compare-int list n nil nil t 'list-length=int?))

(defun list-length<>int? (list n)
  (list-length-compare-int list n t t nil 'list-length<>int?))

(defun list-length>=int? (list n)
  (list-length-compare-int list n nil t t 'list-length>=int?))

(defun list-length>int? (list n)
  (list-length-compare-int list n nil t nil 'list-length>int?))

(defun length<? (left right)
  (etypecase right
    ((or cons null) (list-length<? left right))
    (number (list-length<int? left right))))

(defun length<=? (left right)
  (etypecase right
    ((or cons null) (list-length<=? left right))
    (number (list-length<=int? left right))))

(defun length=? (left right)
  (etypecase right
    ((or cons null) (list-length=? left right))
    (number (list-length=int? left right))))

(defun length<>? (left right)
  (etypecase right
    ((or cons null) (list-length<>? left right))
    (number (list-length<>int? left right))))

(defun length>=? (left right)
  (etypecase right
    ((or cons null) (list-length>=? left right))
    (number (list-length>=int? left right))))

(defun length>? (left right)
  (etypecase right
    ((or cons null) (list-length>? left right))
    (number (list-length>int? left right))))

(defun require-initarg (initarg)
  (error "Required initarg ~s omitted." initarg))

(defun swap-args (f)
  ;; Assumes f is a binary function. (Which is implied by the word "swap".)
  (lambda (a b)
    (funcall f b a)))

(defun trapezoid (f a b)
  (lambda (n)
    (let ((dx (/ (- b a) n)))
      (labels ((l (i sum)
                 (if (>= i n)
                     (* sum dx)
                     (let ((x (+ a (* i dx))))
                       (l (+ i 1) (+ sum (funcall f x)))))))
        (l 1 (/ (+ (funcall f a) (funcall f b)) 2))))))

(defun xcons (cdr car)
  (cons car cdr))
