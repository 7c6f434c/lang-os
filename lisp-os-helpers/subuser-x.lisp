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
    #:reset-bus-helpers
    #:*dbus-helper*
    #:*pulseaudio-helper*
    #:subuser-nsjail-x-application
    ))
(in-package :lisp-os-helpers/subuser-x)

(defvar *firefox-profile-contents* nil)
(defvar *firefox-profile-combiner* nil)
(defvar *firefox-launcher* nil)
(defvar *dbus-helper* nil)
(defvar *pulseaudio-helper* nil)

(defun absolutise-command (command)
  (if (listp (first command))
    (cons (namestring (truename (which (first (first command)))))
          (rest command))
    command))

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
	:name name :uid uid :options options :environment environment
	:display display :x-socket x-socket :system-socket system-socket))
    (with-system-socket
      (system-socket)
      (take-reply-value
	(ask-server
	  (with-uid-auth
	    `(run-as-subuser
	       ,name
	       ,(absolutise-command command)
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

(defun reset-firefox-launcher (&key profile-contents nix-path nix-wrapper-file
                                    out-link verbose)
  (setf *firefox-profile-contents* profile-contents)
  (let
    ((firefox-scripts 
       (namestring
         (truename
           (nix-build
             (format
               nil
               "with import ~s { profileContent = \"\" + ~a; }; firefoxScripts"
               nix-wrapper-file 
               (cl-ppcre:regex-replace
                 "/$" (namestring (truename profile-contents)) ""))
             :nix-path nix-path
             :out-link out-link
             :verbose verbose)))))
    (setf *firefox-profile-combiner* 
          (format nil "~a/bin/~a" firefox-scripts "combine-firefox-profile"))
    (setf *firefox-launcher*
          (format nil "~a/bin/~a" firefox-scripts "firefox-launcher"))))

(defun reset-bus-helpers
  (&key
    (nix-path (cl-ppcre:split ":" (uiop:getenv "NIX_PATH"))) nix-file out-link)
  (setf
    *dbus-helper* 
    (format nil 
            "~a/bin/with-dbus"
            (namestring
              (truename
                (nix-build
                  "withDBus" :nix-file nix-file
                  :nix-path nix-path
                  :out-link (when out-link (format nil "~a-dbus" out-link))))))
    *pulseaudio-helper*
    (format nil 
            "~a/bin/with-pulseaudio"
            (namestring
              (truename
                (nix-build
                  "withPulseaudio" :nix-file nix-file
                  :nix-path nix-path
                  :out-link (when out-link (format nil "~a-pa" out-link))))))
    ))

(defun firefox-pref-value-js (value)
  (cond
    ((null value) "false")
    ((equal t value) "true")
    ((numberp value) value)
    ((stringp value) (format nil "~s" value))
    (t (error "Unrecognised preference type for value ~s" value))))

(defun subuser-name-and-marionette-socket (&key name hostname-hidden-suffix)
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
	       (get-current-user-name)
               (masked-username name hostname-hidden-suffix))))
    (ensure-directories-exist marionette-socket)
    (iolib/syscalls:chmod 
      (directory-namestring marionette-socket)
      #o0700)
    (uiop:run-program
      (list "setfacl" "-m" (format nil "u:~a:rwx" uid)
	    (directory-namestring marionette-socket)))
    `(:name ,name :uid ,uid :marionette-socket ,marionette-socket)))

(defun subuser-nsjail-x-application
  (command
    &key display
    environment home name locale locale-archive
    (slay t) (wait t) (netns t) verbose-netns network-ports grant
    pass-stderr pass-stdout full-dev grab-dri launcher-wrappers
    mounts system-socket setup directory
    hostname hostname-suffix hostname-hidden-suffix
    grab-devices fake-passwd fake-groups grab-sound grab-camera
    (path "/var/current-system/sw/bin") verbose-errors verbose-nsjail
    mount-sys keep-namespaces
    dns http-proxy socks-proxy with-dbus with-pulseaudio)
  (let*
    ((name (or name (timestamp-usec-recent-base36)))
     (uid 
       (take-reply-value
         (with-system-socket
           ()
           (ask-server
             (with-uid-auth
               `(subuser-uid ,name))))))
     (socks-proxy (if (and socks-proxy (not (integerp socks-proxy))) 1080 socks-proxy))
     (http-proxy (if (and socks-proxy (not (integerp socks-proxy))) 3128 http-proxy))
     (hostname-suffix (or hostname-suffix
                          (when (or (eq home t)) "")))
     (hostname (or
                 hostname 
                 (when (or hostname-suffix hostname-hidden-suffix)
                   (cond
                     ((or
                        (eq hostname-suffix t)
                        (equal hostname-suffix ""))
                      (masked-username
                        name hostname-hidden-suffix))
                     (t (format nil "~a.~a"
                                (masked-username 
                                  name hostname-hidden-suffix)
                                (or hostname-suffix "")))))))
     (home (if (eq home t)
             (let* ((containing-directory
                      (format nil "/tmp/subuser-homes-~a/"
                              (get-current-user-name)))
                    (home
                      (progn
                        (ensure-directories-exist containing-directory)
                        (uiop:run-program
                          (list "mktemp" "-d" "-p" containing-directory
                                (concatenate 'string hostname "-XXXXXXXX"))
                          :output (list :string :stripped t)))))
               (uiop:run-program
                 (list "setfacl" "-m" (format nil "u:~a:rwx" uid) home))
               home)
             home))
     )
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
                  for d in (append (when grab-dri (list "/dev/dri/card*" "/dev/dri/render*"))
                                   (when grab-sound (list "/dev/snd/*"))
                                   (when grab-camera (list "/dev/video*"))
                                   (when with-pulseaudio (list "/dev/snd/*"))
                                   grab-devices)
                  for dl := (directory d)
                  for dn := (mapcar 'namestring dl)
                  append dn)
               ,name))))
      (loop
        for g in grant
        do
        (uiop:run-program
          (list "setfacl" "-R" "-m" (format nil "u:~a:rwX" uid) g)))
      (unwind-protect
        (prog1
          (subuser-command-with-x
            `(,@ launcher-wrappers
                 ,@(when with-dbus (list *dbus-helper*))
                 ,@(when with-pulseaudio (list *pulseaudio-helper*))
                 ,@command)
            :setup setup
            :display display :name name
            :environment
            `(
              ,@(when grab-dri
                  `(
                    ("LD_LIBRARY_PATH" "/run/opengl-driver/lib")
                    ))
              ,@ environment
              ,@(when http-proxy
                  `(
                    ("proxy"         ,(format nil "http://127.0.0.1:~a" http-proxy))
                    ("http_proxy"    ,(format nil "http://127.0.0.1:~a" http-proxy))
                    ("https_proxy"   ,(format nil "http://127.0.0.1:~a" http-proxy))
                    ("ftp_proxy"     ,(format nil "http://127.0.0.1:~a" http-proxy))
                    ))
              ,@(when socks-proxy
                  `(
                    ("proxy"         ,(format nil "socks5://127.0.0.1:~a" socks-proxy))
                    ("http_proxy"    ,(format nil "socks5://127.0.0.1:~a" socks-proxy))
                    ("https_proxy"   ,(format nil "socks5://127.0.0.1:~a" socks-proxy))
                    ("ftp_proxy"     ,(format nil "socks5://127.0.0.1:~a" socks-proxy))
                    ("proxy"         ,(format nil "socks5://127.0.0.1:~a" socks-proxy))
                    ("socks_proxy"   ,(format nil "socks5://127.0.0.1:~a" socks-proxy))
                    ("SOCKS_SERVER"  "127.0.0.1")
                    ("SOCKS_PORT"    ,(format nil "~a" socks-proxy))
                    ("SOCKS_VERSION" "5")
                    ))
              ,@(when home `(("HOME" ,home)))
              ("PATH" ,path)
              ("LANG" ,(or locale (uiop:getenv "LANG") "en_US.UTF-8"))
              ("LOCALE_ARCHIVE"
               ,(or
                  locale-archive
                  (uiop:getenv "LOCALE_ARCHIVE")
                  "/run/current-system//sw/lib/locale/locale-archive"))
              )
            :options
            `(
              ,@(when slay `("slay"))
              ,@(when wait `("wait"))
              ("nsjail" "network"
               ,@(when (or with-pulseaudio full-dev) `("full-dev"))
               ,@(when (or with-pulseaudio fake-passwd) `("fake-passwd"))
               ,@(when fake-groups `(("fake-groups" ,fake-groups)))
               ("hostname" ,hostname)
               ("mounts"
                (
                 ,@(when grab-dri `(("-B" "/dev/dri")))
                 ,@(when grab-sound `(("-B" "/dev/snd")))
                 ,@(when (or mount-sys grab-dri) `(("-B" "/sys")))
                 ,@(when home `(("-B" ,home)))
                 ,@ mounts))
               ,@(when verbose-nsjail `("verbose"))
               ,@(when keep-namespaces
                   `(("keep-namespaces" ,keep-namespaces)))
               )
              ,@(when directory `(("directory" ,directory)))
              ,@(when netns
                  `(("netns"
                     (
                      ,@(when dns `( ((53 :udp)) ((53 :tcp))  ))
                      ,@(when http-proxy `(((,http-proxy tcp))))
                      ,@(when socks-proxy `(((,socks-proxy tcp))))
                      ,@network-ports)
                     (
                      ,@(when verbose-netns `("verbose"))))))
              ,@(when pass-stdout `(("stdout-fd" "stdout")))
              ,@(when pass-stderr `(("stderr-fd" "stderr")))
              )
            :system-socket *ambient-system-socket*
            :verbose-errors verbose-errors)
          (ask-server `(close-received-fds)))))))

(defun subuser-firefox
  (arguments
    &rest options
    &key prefs raw-prefs (grab-dri t) write-marionette-socket
    environment marionette-socket profile-storage name
    (firefox-launcher *firefox-launcher*) (slay t) (wait t)
    mounts (hostname-suffix "") hostname-hidden-suffix certificate-overrides socks-proxy
    network-ports keep-namespaces)
  (declare (ignorable options))
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
          (getf (subuser-name-and-marionette-socket 
                  :name name
                  :hostname-hidden-suffix hostname-hidden-suffix)
                :marionette-socket))
         (t marionette-socket)))
     )
    (when marionette-socket
      (ensure-directories-exist marionette-socket)
      (iolib/syscalls:chmod 
        (directory-namestring marionette-socket)
        #o0700)
      (uiop:run-program
        (list "setfacl" "-m" (format nil "u:~a:rwx" uid)
              (directory-namestring marionette-socket)))
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
    (when write-marionette-socket
      (format t "~a~%" marionette-socket))
    (unwind-protect
      (apply
        'subuser-nsjail-x-application
        (cons firefox-launcher arguments)
        :name name :grab-dri grab-dri
        :slay slay :wait wait
        :hostname-suffix hostname-suffix
        :keep-namespaces (append (list "ipc") keep-namespaces)
        :environment
        `(
          ,@ environment
          ("MARIONETTE_SOCKET" ,(or marionette-socket ""))
          ("FIREFOX_PROFILE" ,combined-profile)
          ("FIREFOX_PROFILE_KILL" ,(if profile-storage "" "1"))
          )
        :mounts
        `(
          ("-B" ,combined-profile)
          ,@(when marionette-socket
              `(("-B" ,(directory-namestring marionette-socket))))
          ,@ mounts
          )
        options)
      (when wait
        (ignore-errors
          (uiop:run-program
            (list
              "rm" "-rf" combined-profile)
            :error-output t))
        (when marionette-socket
          (ignore-errors
            (uiop:run-program
              (list "rm" "-rf" (directory-namestring marionette-socket))
              :error-output t)))))))
