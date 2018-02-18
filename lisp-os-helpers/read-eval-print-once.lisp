(defpackage :lisp-os-helpers/read-eval-print-once
  (:use :common-lisp)
  (:export
    #:read-eval-print
    #:package-rep
    #:load-rep
    )
  (:nicknames :rep1))
(in-package :lisp-os-helpers/read-eval-print-once)

(defun read-eval-print ()
  (handler-case (format t "~s~%" (eval (read)))
    (error (e)
	   (format *error-output* "~%~%~a~%" e)
	   (trivial-backtrace:print-backtrace-to-stream *error-output*)
	   (format *error-output* "~a~%" e))))

(defun package-rep (&rest packages)
  (loop for package in packages do (use-package package))
  (read-eval-print))

(defun load-rep (&rest files)
  (loop for f in files do (load f))
  (read-eval-print))
