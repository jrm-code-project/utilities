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
   "CROSS-RATIO"
   "FALLING-FACTORIAL"
   "FOLD-LEFT"
   "FOLD-RIGHT"
   "INTEGER-LOG"
   "LEFTMOST-DIGIT"
   "LENGTH<?"
   "LENGTH<=?"
   "LENGTH=?"   
   "LENGTH<>?"   
   "LENGTH>=?"   
   "LENGTH<?"   
   "LET"
   "NAMED-LAMBDA"
   "RISING-FACTORIAL"
   "SIGMA"
   "SQUARE"
   "XCONS"
   )
  )
