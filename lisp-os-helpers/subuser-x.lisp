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
    display setup verbose-errors
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
		    (t o))))))
        :verbose verbose-errors))))

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
  (arguments &key display prefs raw-prefs launcher-wrappers
             environment marionette-socket home profile-storage name
             (firefox-launcher *firefox-launcher*) (slay t) (wait t)
             (netns t) network-ports pass-stderr pass-stdout full-dev
             mounts system-socket setup hostname grab-devices fake-passwd
             (path "/var/current-system/sw/bin") verbose-errors mount-sys
             certificate-overrides dns http-proxy socks-proxy)
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
    (when certificate-overrides
      (alexandria:write-string-into-file
        (alexandria:read-file-into-string certificate-overrides)
        (format nil "~a/cert_override.txt" combined-profile)
        :if-exists :append :if-does-not-exist :create))
    (uiop:run-program
      (list "setfacl" "-R" "-m" (format nil "u:~a:rwX" uid) combined-profile)
      :error-output t)
    (uiop:run-program
      (list "chmod" "-R" "u=rwX" combined-profile) :error-output t)
    (alexandria:write-string-into-file
      (format
        nil "~{user_pref(\"~a\",~a);~%~}"
        (append
          (loop 
            for pair in
            `(
              ,@ prefs
              ,@ (when socks-proxy
                   `(
                     ("network.proxy.socks" "127.0.0.1")
                     ("network.proxy.socks_port" ,socks-proxy)
                     ("network.proxy.socks_version" 5)
                     ("network.proxy.type" 1)
                     ))
              )
            for name := (first pair)
            for value := (second pair)
            for representation :=
            (firefox-pref-value-js value)
            collect name collect representation)
          (loop
            for pair in raw-prefs
            for name := (first pair)
            for value := (second pair)
            collect name collect value)))
      (format nil "~a/prefs.js" combined-profile)
      :if-exists :append)
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
            `(,@ launcher-wrappers
              ,firefox-launcher ,@arguments)
            :setup setup
            :display display :name name
            :environment
            `(
              ,@ environment
              ,@(when http-proxy
                  `(
                    ("proxy"         "http://127.0.0.1:3128")
                    ("http_proxy"    "http://127.0.0.1:3128")
                    ("https_proxy"   "http://127.0.0.1:3128")
                    ("ftp_proxy"     "http://127.0.0.1:3128")
                    ))
              ,@(when socks-proxy
                  `(
                    ("proxy"         "socks5://127.0.0.1:1080")
                    ("socks_proxy"   "socks5://127.0.0.1:1080")
                    ("SOCKS_SERVER"  "127.0.0.1")
                    ("SOCKS_PORT"    "1080")
                    ("SOCKS_VERSION" "5")
                    ))
              ("MARIONETTE_SOCKET" ,(or marionette-socket ""))
              ,@(when home `(("HOME" ,home)))
              ("FIREFOX_PROFILE" ,combined-profile)
              ("FIREFOX_PROFILE_KILL" ,(if profile-storage "" "1"))
              ("PATH" ,path)
              )
            :options
            `(
              ,@(when slay `("slay"))
              ,@(when wait `("wait"))
              ("nsjail" "network"
               ,@(when full-dev `("full-dev"))
               ,@(when fake-passwd `("fake-passwd"))
	       ("hostname" ,hostname)
               ("mounts"
                (("-B" ,combined-profile)
                 ("-B" "/dev/dri")
		 ,@(when marionette-socket
		     `(("-B" ,(directory-namestring marionette-socket))))
                 ,@(when mount-sys `(("-B" "/sys")))
                 ,@ mounts)))
              ,@(when netns
                  `(("netns"
                     (
                      ,@(when dns `( ((53 :udp)) ((53 :tcp))  ))
                      ,@(when http-proxy `(((3128 tcp))))
                      ,@(when socks-proxy `(((1080 tcp))))
                      ,@network-ports))))
              ,@(when pass-stdout `(("stdout-fd" "stdout")))
              ,@(when pass-stderr `(("stderr-fd" "stderr")))
              )
            :system-socket *ambient-system-socket*
            :verbose-errors verbose-errors)
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
