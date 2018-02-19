(defpackage :lisp-os-helpers/kernel
  (:use
    :common-lisp
    )
  (:export
    #:modprobe
    ))
(in-package :lisp-os-helpers/kernel)

(defun modprobe (module &rest parameters)
  (uiop:run-program
    `("/var/current-system/bin/modprobe"
      ,module
      ,@(loop
          for p in parameters
          collect
          (cond
            ((stringp p) p)
            ((listp p)
             (format nil "~a=~a" (first p) (or (second p) "")))
            (t (format nil "~a" p)))))))
