(defpackage :lisp-os-helpers/util
  (:use :common-lisp)
  (:export
    #:defun-weak
    ))
(in-package :lisp-os-helpers/util)

(defmacro defun-weak (name args &rest body)
  `(unless (fboundp ',name)
     (defun ,name ,args ,@body)))

