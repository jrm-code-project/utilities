;;; -*- Lisp -*-

(in-package "UTILITIES")

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

(defun fold-right (function list final &rest args)
  (labels ((fold-right-1 (tail final)
             (cond ((consp tail)
                    (funcall function (car tail) (fold-right-1 (cdr tail) final)))
                   ((null tail) final)
                   (t (error "Dotted list encountered by fold-right."))))

           (fold-right-n (tails final)
             (cond ((every #'consp tails)
                    (apply function (append (map 'list #'car tails) (list (fold-right-n (map 'list #'cdr tails) final)))))
                   ((every #'null tails)
                    final)
                   (t (error "Lists of different lengths or dotted list in fold-right.")))))
    (if (null args)
        (fold-right-1 list final)
        (fold-right-n (list* list final (butlast args)) (car (last args))))))

(defun integer-log (n base)
  "Returns two values, the integer-log of <n> in <base>, and the leftmost digit
in <base>."
  (if (< n base)
      (values 0 n)
      (multiple-value-bind (ilog l) (integer-log n (* base base))
        (if (< l base)
            (values (* ilog 2) l)
            (values (+ (* ilog 2) 1) (/ l base))))))

(defun leftmost-digit (n base)
  (if (< n base)
      n
      (let ((l (leftmost-digit n (* base base))))
        (if (< l base)
            l
            (truncate l base)))))

(defun square (number)
  "Square a number by multiplying it by itself."
  (* number number))

(defun xcons (cdr car)
  (cons car cdr))
