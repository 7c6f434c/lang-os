(defpackage :lisp-os-helpers/safe-read
  (:use :common-lisp)
  (:export
    #:safe-read
    #:check-safety))
(in-package :lisp-os-helpers/safe-read)

(define-condition unacceptable-symbol (simple-error)
  ((symbol :initarg :symbol :reader unacceptable-symbol-content)
   (package :initarg :package :reader unacceptable-symbol-desired-package))
  (:report (lambda (condition stream)
             (if (unacceptable-symbol-desired-package condition)
               (format stream "Symbol ~s is not in the expected package ~a."
                       (unacceptable-symbol-content condition)
                       (unacceptable-symbol-desired-package condition))
               (format stream "Symbol ~s when no symbols allowed."
                       (unacceptable-symbol-content condition))))))

(defun check-safety (form packages)
  (let*
    ((packages (mapcar 'find-package packages)))
    (cond
      ((consp form)
       (check-safety (car form) packages)
       (check-safety (cdr form) packages))
      ((null form))
      ((symbolp form)
       (unless
         (loop
           for p in packages
           when (eq (find-symbol (string form) p) form)
           return t)
	 (format t "~s~%" (list form packages (null form)))
         (error
           (make-condition
             'unacceptable-symbol
             :symbol form :package (first packages)))))
      (t ))
    form))

(defun safe-read
  (source packages &key forbid-nil)
  (if (stringp source)
    (with-input-from-string (s source)
      (safe-read s packages))
    (let*
      ((*read-eval* nil)
       (packages (remove nil (mapcar 'find-package packages)))
       (*package* (if packages (first packages) (make-package (format nil "~36r" (random (expt 36 40))))))
       (form (progn (unless (or packages forbid-nil)
		      (import (list nil) *package*))
		    (check-safety (read source) packages))))
      (unless packages (delete-package *package*))
      form)))
