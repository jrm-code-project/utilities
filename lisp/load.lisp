;;; -*- Lisp -*-

(setf (logical-pathname-translations "UTILS")
      (list (list "**;*.*" (make-pathname :directory
                                          (append
                                           (pathname-directory *load-pathname*) '(:wild-inferiors))
                                          :name :wild
                                          :type :wild
                                          :version :wild))))

;;; Load up the packages first
(map nil #'load
     '(
       "UTILS:UTILITIES;UTILITIES.PACKAGE"

       "UTILS:PROMISE;PROMISE.PACKAGE"
       "UTILS:STREAM;STREAM.PACKAGE"
       ))

(map nil #'load
     '(
       "UTILS:UTILITIES;UTILITIES"

       "UTILS:PROMISE;PROMISE"
       "UTILS:STREAM;STREAM"
       ))


