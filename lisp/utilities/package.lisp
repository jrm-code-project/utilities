;;; -*- Lisp -*-

(defpackage "UTILITIES"
  (:nicknames "UTILS")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          ;; "LET"
                          "LET*"
                          "MULTIPLE-VALUE-BIND")
  (:shadow "LET")
  (:use "COMMON-LISP" "SERIES")
  (:export
   "AVERAGE"
   "BIG-PI"
   "BIG-SIGMA"
   "BINOMIAL"
   "CROSS-RATIO"
   "CUBE"
   "FACTORIAL"
   "FALLING-FACTORIAL"
   "FOLD-LEFT"
   "FOLD-RIGHT"
   "HARMONIC"
   "INTEGER-LOG"
   "LEAST-SQUARES"
   "LEFTMOST-DIGIT"
   "LENGTH<?"
   "LENGTH<=?"
   "LENGTH=?"   
   "LENGTH<>?"   
   "LENGTH>=?"   
   "LENGTH<?"   
   "LET"
   "NAMED-LAMBDA"
   "REQUIRE-INITARG"
   "RISING-FACTORIAL"
   "SQUARE"
   "SWAP-ARGS"
   "XCONS"
   )
  )
