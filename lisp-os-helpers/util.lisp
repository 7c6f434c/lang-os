(defpackage :lisp-os-helpers/util
  (:use :common-lisp)
  (:export
    #:defun-weak
    #:getf-fun
    ))
(in-package :lisp-os-helpers/util)

(defmacro defun-weak (name args &rest body)
  `(unless (fboundp ',name)
     (defun ,name ,args ,@body)))

(defun getf-fun (key) (lambda (obj) (getf obj key)))
