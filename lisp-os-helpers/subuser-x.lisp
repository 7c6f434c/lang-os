(defpackage :lisp-os-helpers/subuser-x
  (:use
    :common-lisp
    :lisp-os-helpers/shell
    :lisp-os-helpers/nix
    :lisp-os-helpers/timestamp
    :lisp-os-helpers/socket-command-client)
  (:export
    #:reset-firefox-launcher
    #:subuser-command-with-x
    #:subuser-firefox
    #:firefox-pref-value-js
    #:subuser-name-and-marionette-socket
    #:*firefox-launcher*
    ))
(in-package :lisp-os-helpers/subuser-x)

(defvar *firefox-profile-contents* nil)
(defvar *firefox-profile-combiner* nil)
(defvar *firefox-launcher* nil)

(defun
  subuser-command-with-x
  (command 
    &key
    display setup
    name environment options system-socket)
  (let*
    ((display
       (or
	 display
	 (ignore-errors
	   (parse-integer
	     (subseq (uiop:getenv "DISPLAY") 1)))
	 0))
     (name (or name (timestamp-usec-recent-base36)))
     (uid 
       (take-reply-value 
	 (with-system-socket
	   ()
	   (ask-server (with-uid-auth `(subuser-uid ,name))))))
     (x-socket (format nil "/tmp/.X11-unix/X~a" display))
     )
    (uiop:run-program
      (list "setfacl" "-m" (format nil "u:~a:rw" uid)
	    x-socket))
    (when setup
      (funcall
	setup
	:allow-other-keys t
	:name name :uid uid
	:display display :x-socket x-socket :system-socket system-socket))
    (with-system-socket
      (system-socket)
      (take-reply-value
	(ask-server
	  (with-uid-auth
	    `(run-as-subuser
	       ,name
	       ,command
	       (,@environment
		 ("DISPLAY" ,(format nil ":~a" display)))
	       ,(loop
		  for o in options
		  collect
		  (cond
		    ((and (listp o) (equalp (string (first o)) "nsjail"))
		     (append
		       (list "nsjail")
		       (loop
                         with result := nil
                         with mounts-seen := nil
                         for oo in (rest o)
                         do
                         (push
                           (cond
                             ((and (listp oo)
                                   (equalp (string (first oo)) "mounts"))
                              (setf mounts-seen t)
                              (list
                                "mounts"
                                (append
                                  (second oo)
                                  (list (list "-B" x-socket x-socket)))))
                             (t oo))
                           result)
                         finally
                         (progn
                           (unless mounts-seen
			     (push (list "mounts"
					 (list (list "-B" x-socket x-socket)))
				   result))
                           (return (reverse result))))))
		    (t o))))))))))

(defun reset-firefox-launcher (&key profile-contents nix-path nix-wrapper-file)
  (setf *firefox-profile-contents* profile-contents)
  (let
    ((firefox-scripts 
       (nix-build
	 (format
	   nil
	   "with import ~s { profileContent = \"\" + ~a; }; firefoxScripts"
	   nix-wrapper-file 
	   (cl-ppcre:regex-replace "/$" (namestring (truename profile-contents)) ""))
	 :nix-path nix-path)))
    (setf *firefox-profile-combiner* 
          (format nil "~a/bin/~a" firefox-scripts "combine-firefox-profile"))
    (setf *firefox-launcher*
          (format nil "~a/bin/~a" firefox-scripts "firefox-launcher"))))

(defun firefox-pref-value-js (value)
  (cond
    ((null value) "false")
    ((equal t value) "true")
    ((numberp value) value)
    ((stringp value) (format nil "~s" value))
    (t (error "Unrecognised preference type for value ~s" value))))

(defun subuser-name-and-marionette-socket (&key name)
  (let*
    ((name (or name (timestamp-usec-recent-base36)))
     (uid (take-reply-value
	    (with-system-socket
	      ()
	      (ask-server
		(with-uid-auth
		  `(subuser-uid ,name))))))
     (marionette-socket
       (format nil "/tmp/ff.~a/sockets/~a/marionette-socket"
	       (get-current-user-name) name)))
    (ensure-directories-exist marionette-socket)
    (iolib/syscalls:chmod 
      (directory-namestring marionette-socket)
      #o0700)
    (uiop:run-program
      (list "setfacl" "-m" (format nil "u:~a:rwx" uid)
	    (directory-namestring marionette-socket)))
    `(:name ,name :uid ,uid :marionette-socket ,marionette-socket)))


(defun subuser-firefox
  (arguments &key display prefs
             environment marionette-socket home profile-storage name
             (firefox-launcher *firefox-launcher*) (slay t) (wait t)
             (netns t) network-ports pass-stderr pass-stdout full-dev
             mounts system-socket setup hostname grab-devices
             (path "/var/current-system/sw/bin"))
  (let*
    (
     (combined-profile
       (uiop:run-program
         (list *firefox-profile-combiner* (or profile-storage ""))
         :output '(:string :stripped t) :error-output t))
     (name (or name (timestamp-usec-recent-base36)))
     (uid (take-reply-value
	    (with-system-socket 
	      ()
	      (ask-server (with-uid-auth `(subuser-uid ,name))))))
     (marionette-socket
       (case marionette-socket
	 ((t)
	  (getf (subuser-name-and-marionette-socket :name name)
		:marionette-socket))
	 (t marionette-socket)))
     (hostname
       (or hostname (format nil "~a.~a" (get-current-user-name) name)))
     )
    (uiop:run-program
      (list "setfacl" "-R" "-m" (format nil "u:~a:rwX" uid) combined-profile)
      :error-output t)
    (uiop:run-program
      (list "chmod" "-R" "u=rwX" combined-profile) :error-output t)
    (with-system-socket
      (system-socket)
      (when pass-stdout
        (send-fd-over-unix-socket
          (concatenate
            'string (string #\Null)
            (take-reply-value (ask-server `(fd-socket))))
          1)
        (ask-server `(receive-fd stdout)))
      (when pass-stderr
        (send-fd-over-unix-socket
          (concatenate
            'string (string #\Null)
            (take-reply-value (ask-server `(fd-socket))))
          2)
        (ask-server `(receive-fd stderr)))
      (take-reply-value
	(ask-server 
	  (with-uid-auth
	    `(grab-devices
	       ,(loop
		   for d in (append (list "/dev/dri/card*") grab-devices)
		   for dl := (directory d)
		   for dn := (mapcar 'namestring dl)
		   append dn)
	       ,name))))
      (unwind-protect
        (prog1
          (subuser-command-with-x
            `(,firefox-launcher ,@arguments)
            :setup setup
            :display display :name name
            :environment
            `(
              ,@ environment
              ("MARIONETTE_SOCKET" ,(or marionette-socket ""))
              ("HOME" ,(or home ""))
              ("FIREFOX_PROFILE" ,combined-profile)
	      ("FIREFOX_PROFILE_KILL" ,(if profile-storage "" "1"))
              ("FIREFOX_EXTRA_PREFS"
               ,(format
                  nil "~{user_pref(\"~a\",~a);~%~}"
                  (loop 
                    for pair in prefs
                    for name := (first pair)
                    for value := (second pair)
                    for representation :=
                    (firefox-pref-value-js value)
		    collect name collect representation)))
              ("PATH" ,path)
              )
            :options
            `(
              ,@(when slay `("slay"))
              ,@(when wait `("wait"))
              ("nsjail" "network"
               ,@(when full-dev `("full-dev"))
	       ("hostname" ,hostname)
               ("mounts"
                (("-B" ,combined-profile)
                 ("-B" "/dev/dri")
		 ,@(when marionette-socket
		     `(("-B" ,(directory-namestring marionette-socket))))
                 ,@ mounts)))
              ,@(when netns `(("netns" ,network-ports)))
              ,@(when pass-stdout `(("stdout-fd" "stdout")))
              ,@(when pass-stderr `(("stderr-fd" "stderr")))
              )
            :system-socket *ambient-system-socket*)
          (ask-server `(close-received-fds)))
        (ignore-errors
          (uiop:run-program
            (list
              "rm" "-rf" combined-profile)
	    :error-output t))
	(when marionette-socket
	  (ignore-errors
	    (uiop:run-program
	      (list "rm" "-rf" (directory-namestring marionette-socket))
	      :error-output t)))
	))))
