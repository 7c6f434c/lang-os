(defpackage :lisp-os-helpers/references
  (:use :common-lisp)
  (:export #:with-references #:with-reference))

(in-package :lisp-os-helpers/references)

(defmacro with-reference ((name &optional hash-table-test) &body body)
  (let*
    ((real-name (gensym))
     (wrapper-name (gensym))
     (reference (if hash-table-test `(gethash key ,real-name) real-name)))
    `(let
       ((,wrapper-name
          (let ((,real-name
                  ,(when hash-table-test
                     `(make-hash-table :test ',hash-table-test))))
            (lambda (,@(when hash-table-test `(key))
                      &optional (value nil valuep))
              (if valuep (setf ,reference value) ,reference)))))
       (flet ((,name (&rest args) (apply ,wrapper-name args)))
         ,@body))))
(defmacro with-references ((&rest names) &body body)
  (cond
    ((consp (first names))
     `(with-reference
        ,(first names)
        (with-references
          ,(cdr names)
          ,@body)))
    (names
      `(with-reference
         (,(first names))
         (with-references
           ,(cdr names)
           ,@body)))
    ((= (length body) 1) (first body))
    (t `(progn ,@body))))

