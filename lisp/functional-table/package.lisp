;;; -*- Lisp -*-

(defpackage "FUNCTIONAL-TABLE"
  (:nicknames "FTABLE")
  (:shadow "APPEND")
  (:shadowing-import-from "UTILITIES" "LET")
  (:use "COMMON-LISP" "UTILITIES")
  (:export
   "APPEND"
   "EMPTY"
   "EXTEND"
   "EXTEND*"
   "LOOKUP"
   "REDACT"
   "REDACT*"
   ))
