(defpackage :lisp-os-helpers/ffi
  (:use :common-lisp)
  (:export
    #:adjust-ffi-paths
    ))
(in-package :lisp-os-helpers/ffi)

(defun adjust-ffi-paths (&rest paths)
  (let* (
         (old-cffi-path (or nil #+cffi cffi:*foreign-library-directories*))
         (env-path 
           (loop
             for name in (list "NIX_LD_LIBRARY_PATH" "LD_LIBRARY_PATH")
             for value := (uiop:getenv name)
             for entries := (uiop:split-string value :separator ":")
             for real-entries := 
             (remove-if (lambda (x) (= (length x) 0))
                        entries)
             append real-entries))
         (full-path 
           (mapcar 
             'namestring 
             (append paths env-path old-cffi-path)))
         )
    #+cffi (setf cffi:*foreign-library-directories* 
                 (loop for d in full-path
                       for td := (ignore-errors (truename d))
                       when td collect td))
    #+sbcl (loop
             for l in sb-alien::*shared-objects*
             for ns := (sb-alien::shared-object-namestring l)
             do (format *error-output* "Searching alien object ~s in ~s~%"
                               ns full-path)
             do (and (> (length ns) 0) (not (equal (elt ns 0) (elt "/" 0)))
                     (let*
                       ((prefix (find-if (lambda (s) 
                                           (probe-file 
                                             (format nil "~a/~a" s ns))) 
                                         full-path))
                        (fullpath (and prefix (format nil "~a/~a" prefix ns))))
                       (when fullpath
                         (format *error-output* "Found: ~s~%" fullpath)
                         (setf
                           (sb-alien::shared-object-namestring l) fullpath
                           (sb-alien::shared-object-pathname l) (probe-file fullpath)))))
             )
    ))
