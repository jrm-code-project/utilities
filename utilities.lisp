;;; -*- Lisp -*-

(in-package "UTILITIES")

(defun average (a b)
  (/ (+ a b) 2))

;;; Limit is exclusive.
(defun big-pi (func start limit)
  (collect-product
   (map-fn 'number func
           (scan-range :from start :below limit))))

;;; Limit is exclusive.
(defun big-sigma (func start limit)
  (collect-sum
   (map-fn 'number func
           (scan-range :from start :below limit))))

(defun binomial (n k)
  (cond ((zerop k) 1)
        ((= n k) 1)
        (t (+ (binomial (- n 1) (- k 1))
              (binomial (- n 1) k)))))

(defun cross-ratio (a b c d)
  (/ (* (- a c) (- b d))
     (* (- a d) (- b c))))

(defun factorial (x)
  (big-pi #'identity 1 (+ x 1)))

(defun falling-factorial (x fall)
  (big-pi #'identity (- x (- fall 1)) (+ x 1)))

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

(defun iota (n)
  (do ((i (- n 1) (- i 1))
       (answer '() (cons i answer)))
      ((minusp i) answer)))

(defun rising-factorial (x rise)
  (big-pi #'identity x (+ x rise)))

(defun least-squares (points)
  (do ((points points (cdr points))
       (sigma-x 0 (+ sigma-x (caar points)))
       (sigma-y 0 (+ sigma-y (cdar points)))
       (sigma-x-squared 0 (+ sigma-x-squared (* (caar points)  (caar points))))
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

(defun symbol-paste (strings package)
  (intern
   (apply #'concatenate 'string (map 'list #'string strings))
   package))

(defun trapezoid (f a b)
  (lambda (n)
    (let ((dx (/ (- b a) n)))
      (labels ((l (i sum)
                 (if (>= i n)
                     (* sum dx)
                     (let ((x (+ a (* i dx))))
                       (l (+ i 1) (+ sum (funcall f x)))))))
        (l 1 (/ (+ (funcall f a) (funcall f b)) 2))))))
