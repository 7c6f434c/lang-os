(defpackage :lisp-os-helpers/auth-data
  (:use :common-lisp)
  (:export
    #:check-password
    #:set-password
    #:*pty-helper*
    #:*auth-challenge-directory*
    #:create-file-challenge
    #:verify-file-challenge
    ))
(in-package :lisp-os-helpers/auth-data)

(defvar *pty-helper* "/run/current-system/sw/bin/in-pty")

(defun check-password (user password &key (pty-helper *pty-helper*))
  (let*
    ((uid (iolib/syscalls:getuid))
     (command 
       (if (= uid 0)
         (list
           pty-helper "env" "LANG=C" "LC_ALL=C" "su" "nobody" "-s" "/bin/sh" "-c"
           (format nil "/run/wrappers/bin/su ~a -s /bin/sh -c true" user))
         (list pty-helper "env" "LANG=C" "LC_ALL=C" "/run/wrappers/bin/su" user "-s" "/bin/sh" "-c" "true")))
     (su-process (uiop:launch-program command :input :stream :output :stream))
     (su-in (uiop:process-info-input su-process))
     (su-out (uiop:process-info-output su-process)))
    (loop
      for c := (read-char su-out nil)
      for s := (string c)
      then (if (find c '(#\Newline #\Return))
	     "" (concatenate 'string s (string c)))
      while (and c (not (equal s "Password:"))))
    (ignore-errors (format su-in "~a~%" password)
		   (close su-in))
    (prog1
      (= 0 (uiop:wait-process su-process))
      (close su-out))))

(defun set-password (user password &key (pty-helper *pty-helper*))
  (let*
    ((uid (iolib/syscalls:getuid)))
    (unless (= uid 0) (error "root access required"))
    (let*
      ((passwd-process
         (uiop:launch-program
           (list
             pty-helper
             "/run/current-system/bin/system-passwd" user)
           :input :stream :output :stream))
       (passwd-in (uiop:process-info-input passwd-process))
       (passwd-out (uiop:process-info-output passwd-process)))
      (loop
        for k from 1 to 2
        do
        (progn
          (loop
            with line := nil
            for c := (read-char passwd-out nil)
            do (when c (push c line))
            unless c do
            (format
              *error-output* "Weird data from passwd:~%~a~%"
              (map 'string 'identity (reverse line)))
            while (and c (not (equal c #\:))))
          (format passwd-in "~a~%" password)
          (finish-output passwd-in)))
      (unwind-protect
        (= 0 (uiop:wait-process passwd-process))
        (ignore-errors (close passwd-in))
        (ignore-errors (close passwd-out))))))

(defvar *auth-challenge-directory* "/run/auth-challenges")

(defun create-file-challenge (&key users groups (auth-challenge-directory *auth-challenge-directory*))
  (uiop:with-temporary-file
    (:pathname path :stream f :keep t :directory auth-challenge-directory)
    (uiop:run-program (list "chmod" "0" (namestring path)))
    (format f "~36r~%" (random (expt 36 20)))
    (force-output f)
    (loop
      for user in users
      do (uiop:run-program
           (list "setfacl" "-m"
                 (format nil "u:~a:r" user)
                 (namestring path))))
    (loop
      for group in groups
      do (uiop:run-program
           (list "setfacl" "-m"
                 (format nil "g:~a:r" group)
                 (namestring path))))
    (namestring path)))

(defun verify-file-challenge (path answer)
  (uiop:run-program (list "chmod" "u=r" path))
  (with-open-file (f path)
    (prog1
      (equal (read-line f nil nil) answer)
      (uiop:run-program (list "rm" path)))))
