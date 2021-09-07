;;; -*- Lisp -*-

(defpackage "STREAM"
  (:shadow "STREAM")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET"
                          "LET*"
                          "MULTIPLE-VALUE-BIND")
  (:use "COMMON-LISP" "PROMISE" "SERIES" "UTILITIES")
  (:export
   "CONS-STREAM"
   "DOUBLE-STREAM"
   "EMPTY-STREAM?"
   "EVENS"
   "INTEGERS"
   "NATURALS"
   "ODDS"
   "ONES"
   "RDERIV"
   "ROMBERG"
   "SQUARES"
   "STREAM"
   "STREAM-ACCUMULATE"
   "STREAM-CAR"
   "STREAM-CDR"
   "STREAM-DELAYED-CDR"
   "STREAM-MAP"
   "THE-EMPTY-STREAM"
   )
  )