(defpackage :lisp-os-helpers/util
  (:use :common-lisp)
  (:export
    #:defun-weak
    #:getf-fun
    #:random-number
    ))
(in-package :lisp-os-helpers/util)

(defmacro defun-weak (name args &rest body)
  `(unless (fboundp ',name)
     (defun ,name ,args ,@body)))

(defun getf-fun (key) (lambda (obj) (getf obj key)))

(defun random-bytes (n)
  (with-open-file (f "/dev/urandom" :element-type '(unsigned-byte 8))
    (let ((res (make-array (list n))))
      (read-sequence res f)
      res)))

(defun random-bytes-number (n)
  (loop for k from 1 to n
        for b across (random-bytes n)
        for sum := b then (logior (ash sum 8) b)
        finally (return sum)))

(defun random-number (range)
  (let* ((log (log range 256))
         (bytes (ceiling log))
         (input-range (expt 256 bytes))
         (bytes (if (= input-range range) bytes (+ 10 bytes)))
         (input-range (expt 256 bytes))
         (max-acceptable (- input-range (mod input-range range))))
    (loop for input := (random-bytes-number bytes)
          for result := (mod input range)
          when (< input max-acceptable) return result)))
