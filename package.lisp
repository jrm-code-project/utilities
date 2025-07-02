;;; -*- Lisp -*-

(defpackage "UTILITIES"
  (:nicknames "UTILS")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET"
                          "LET*"
                          "MULTIPLE-VALUE-BIND")
  (:use "COMMON-LISP" "SERIES")
  (:export
   "AVERAGE"
   "BIG-PI"
   "BIG-SIGMA"
   "BINOMIAL"
   "CROSS-RATIO"
   "FACTORIAL"
   "FALLING-FACTORIAL"
   "HARMONIC"
   "INTEGER-LOG"
   "IOTA"
   "LEAST-SQUARES"
   "LEFTMOST-DIGIT"
   "LENGTH<?"
   "LENGTH<=?"
   "LENGTH=?"
   "LENGTH<>?"
   "LENGTH>=?"
   "LENGTH<?"
   "RISING-FACTORIAL"
   "SYMBOL-PASTE"
   )
  )
