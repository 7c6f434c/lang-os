(defpackage :lisp-os-helpers/unix-users
  (:use :common-lisp)
  (:import-from :iolib/syscalls
		#:getpwnam
		#:getgrnam
		)
  (:export
    #:getpwnam
    #:getgrnam
    #:add-daemon-group
    #:add-daemon-user
    #:ensure-daemon-user
    #:ensure-daemon-group
    #:grant-to-user
    #:grant-to-group
    ))
(in-package :lisp-os-helpers/unix-users)

(defun add-daemon-group (name)
  (uiop:run-program
    (list "groupadd" "-r" "-R" "/var/auth" name)))

(defun add-daemon-user (name)
  (uiop:run-program
    (list "useradd" "-R" "/var/auth" "-r" "-g" name "-s" "/run/current-system/sw/bin/nologin" "-d" "/var/empty" name)))

(defun ensure-daemon-user (name)
  (unless (getgrnam name) (add-daemon-group name))
  (unless (getpwnam name) (add-daemon-user name)))

(defun ensure-daemon-group (name)
  (unless (getgrnam name) (add-daemon-group name)))

(defun grant-to-user (name path)
  (uiop:run-program (list "chown" name path)))

(defun grant-to-group (name path)
  (uiop:run-program (list "chgrp" name path)))
