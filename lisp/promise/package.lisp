;;; -*- Lisp -*-

(cl:defpackage "PROMISE"
  (:use "COMMON-LISP")
  (:export "DELAY"
           "FORCE"
           "FORCED?"
           "PROMISE"
           "PROMISE-VALUES"))
