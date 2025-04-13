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
    #:*owned-home-helper*
    #:subuser-nsjail-x-application
    ))
(in-package :lisp-os-helpers/subuser-x)

(defvar *firefox-profile-contents* nil)
(defvar *firefox-profile-combiner* nil)
(defvar *firefox-launcher* nil)
(defvar *dbus-helper* nil)
(defvar *pulseaudio-helper* nil)
(defvar *owned-home-helper* nil)

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
    name environment options system-socket
    x-optional x-forbidden)
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
     (x-socket (unless
                 (or x-forbidden
                     (and x-optional (not (probe-file x-socket))))
                 x-socket))
     )
    (when x-socket
      (uiop:run-program
        (list "setfacl" "-m" (format nil "u:~a:rw" uid)
              x-socket)))
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
                                  (when x-socket
                                    (list (list "-B" x-socket x-socket))))))
                             (t oo))
                           result)
                         finally
                         (progn
                           (unless (or mounts-seen (not x-socket))
                             (push (list "mounts"
                                         (list (list "-B" x-socket x-socket)))
                                   result))
                           (return (reverse result))))))
                    (t o))))))
        :verbose verbose-errors))))

(defun reset-firefox-launcher (&key profile-contents nix-path nix-wrapper-file
                                    out-link drv-link verbose fast)
  (setf *firefox-profile-contents* profile-contents)
  (let
    ((firefox-scripts 
       (namestring
         (truename
           (if (and out-link fast)
             (namestring (truename out-link))
             (nix-build
               (format
                 nil
                 "with import ~s { profileContent = \"\" + ~a; }; firefoxScripts"
                 nix-wrapper-file 
                 (cl-ppcre:regex-replace
                   "/$" (namestring (truename profile-contents)) ""))
               :nix-path nix-path
               :out-link out-link
               :drv-link drv-link
               :verbose verbose))))))
    (setf *firefox-profile-combiner* 
          (format nil "~a/bin/~a" firefox-scripts "combine-firefox-profile"))
    (setf *firefox-launcher*
          (format nil "~a/bin/~a" firefox-scripts "firefox-launcher"))))

(defun reset-bus-helpers
  (&key
    (nix-path (cl-ppcre:split ":" (uiop:getenv "NIX_PATH"))) nix-file
    out-link drv-link fast)
  (let (
        (dbus-out-link (when out-link (format nil "~a-dbus" out-link)))
        (pulseaudio-out-link (when out-link (format nil "~a-pa" out-link)))
        (owned-home-out-link (when out-link (format nil "~a-owned-home" out-link)))
        (dbus-drv-link (when drv-link (format nil "~a-dbus" drv-link)))
        (pulseaudio-drv-link (when drv-link (format nil "~a-pa" drv-link)))
        (owned-home-drv-link (when drv-link (format nil "~a-owned-home" drv-link)))
        dbus-helper pulseaudio-helper owned-home-helper
        )
    (if (and fast out-link)
      (setf 
        dbus-helper dbus-out-link
        pulseaudio-helper pulseaudio-out-link
        owned-home-helper owned-home-out-link
        )
      (setf 
        dbus-helper
        (nix-build
          "withDBus" :nix-file nix-file
          :nix-path nix-path
          :out-link dbus-out-link
          :drv-link dbus-drv-link
          )
        pulseaudio-helper
        (nix-build
          "withPulseaudio" :nix-file nix-file
          :nix-path nix-path
          :out-link pulseaudio-out-link
          :drv-link pulseaudio-drv-link
          )
        owned-home-helper
        (nix-build
          "withOwnedHome" :nix-file nix-file
          :nix-path nix-path
          :out-link owned-home-out-link
          :drv-link owned-home-drv-link
          )
        )
      )
    (setf
      *dbus-helper* 
      (format nil 
              "~a/bin/with-dbus"
              (namestring
                (truename
                  dbus-helper)))
      *pulseaudio-helper*
      (format nil 
              "~a/bin/with-pulseaudio"
              (namestring
                (truename
                  pulseaudio-helper)))
      *owned-home-helper*
      (format nil 
              "~a/bin/with-owned-home"
              (namestring
                (truename
                  owned-home-helper)))
      )
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
    environment home inner-home tmp name locale locale-archive
    (slay t) (wait t) (netns t) verbose-netns
    network-ports network-ports-in
    netns-tuntap-devices
    grant grant-read grant-single grant-read-single
    pass-stderr pass-stdout pass-stdin 
    full-dev fhs-current-system fhs-from
    dev-log-socket
    newprivs
    grab-dri launcher-wrappers
    mounts system-socket setup directory cd-home
    hostname hostname-suffix hostname-hidden-suffix
    grab-devices fake-passwd fake-groups fake-usernames grab-sound grab-camera
    resolv-conf machine-id
    (path "/var/current-system/sw/bin") verbose-errors verbose-nsjail
    mount-sys keep-namespaces
    dns http-proxy socks-proxy with-dbus with-pulseaudio with-owned-home
    x-optional skip-nsjail masking-mounts clear-env
    (proc-rw t) (no-proc nil) (pass-input-config t))
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
                          (when (or (eq home t) (eq tmp t)) "")))
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
     (setup-timestamp (timestamp-usec-recent-base36))
     (home (if (eq home t)
             (let* ((containing-directory
                      (format nil "/tmp/subhomes-~a/"
                              (get-current-user-name)))
                    (home
                      (progn
                        (ensure-directories-exist containing-directory)
                        (uiop:run-program
                          (list "mktemp" "-d" "-p" containing-directory
                                (concatenate 'string 
                                             setup-timestamp 
                                             "-" hostname "-XXXXXXXX"))
                          :output (list :string :stripped t)))))
               (uiop:run-program
                 (list "setfacl" "-m" (format nil "u:~a:rwx" uid) home))
               home)
             home))
     (inner-home (when home (or inner-home home)))
     (directory (if (and inner-home cd-home (not directory)) inner-home directory))
     (tmp (if (eq tmp t)
            (let* ((containing-directory
                     (format nil "/tmp/subtmps-~a/"
                             (get-current-user-name)))
                   (tmp
                     (progn
                       (ensure-directories-exist containing-directory)
                       (uiop:run-program
                         (list "mktemp" "-d" "-p" containing-directory
                               (concatenate 'string setup-timestamp 
                                            "-" hostname "-XXXXXXXX"))
                         :output (list :string :stripped t)))))
              (uiop:run-program
                (list "setfacl" "-m" (format nil "u:~a:rwx" uid) tmp))
              tmp)
            tmp))
     )
    (with-system-socket
      (system-socket)
      (when pass-stdin
        (send-fd-over-unix-socket
          (concatenate
            'string (string #\Null)
            (take-reply-value (ask-server `(fd-socket))))
          0)
        (ask-server `(receive-fd stdin)))
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
        for g in grant-read
        do
        (uiop:run-program
          (list "setfacl" "-R" "-m" (format nil "u:~a:rX" uid) g)))
      (loop
        for g in grant
        do
        (uiop:run-program
          (list "setfacl" "-R" "-m" (format nil "u:~a:rwX" uid) g)))
      (loop
        for g in grant-read-single
        do
        (uiop:run-program
          (list "setfacl" "-m" (format nil "u:~a:rX" uid) g)))
      (loop
        for g in grant-single
        do
        (uiop:run-program
          (list "setfacl" "-m" (format nil "u:~a:rwX" uid) g)))
      (unwind-protect
        (prog1
          (subuser-command-with-x
            `(,@ launcher-wrappers
                 ,@(when with-owned-home (list *owned-home-helper*))
                 ,@(when with-dbus (list *dbus-helper*))
                 ,@(when with-pulseaudio (list *pulseaudio-helper*))
                 ,@command)
            :setup setup
            :display display :name name
            :x-optional x-optional
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
                    ("wss_proxy"     ,(format nil "http://127.0.0.1:~a" http-proxy))
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
              ,@(when inner-home `(("HOME" ,inner-home)))
              ,@(when pass-input-config
                  `(
                    ("GTK_IM_MODULE" ,(or (uiop:getenv "GTK_IM_MODULE") ""))
                    ("QT_IM_MODULE" ,(or (uiop:getenv "QT_IM_MODULE") ""))
                    ))
              ,@(when (and pass-input-config (> (length (uiop:getenv "XCOMPOSEFILE")) 0))
                  `(
                    ("XCOMPOSEFILE" "/_xcomposefile" )
                    ))
              ("PATH" ,path)
              ("LANG" ,(or locale (uiop:getenv "LANG") "C.UTF-8"))
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
              ,@(unless skip-nsjail `(("nsjail" "network"
               ,@(when (or with-pulseaudio full-dev) `("full-dev"))
               ,@(when fhs-current-system `("fhs-current-system"))
               ,@(when fhs-from `(("fhs-from" ,fhs-from)))
               ,@(when (or dev-log-socket) `(("dev-log-socket" ,dev-log-socket)))
               ,@(when (or with-pulseaudio fake-passwd) `("fake-passwd"))
               ,@(when fake-groups `(("fake-groups" ,fake-groups)))
               ,@(when fake-usernames `(("fake-usernames" ,fake-usernames)))
               ,@(when (or resolv-conf) `(("resolv-conf" ,resolv-conf)))
               ,@(when (or machine-id) `(("machine-id" ,machine-id)))
               ("hostname" ,hostname)
               ("mounts"
                (
                 ,@(when grab-dri `(("-B" "/dev/dri")))
                 ,@(when grab-sound `(("-B" "/dev/snd")))
                 ,@(when (or mount-sys grab-dri) `(("-B" "/sys")))
                 ,@(when tmp `(("-B" ,tmp "/tmp")))
                 ,@(when home `(("-B" ,home ,inner-home)("-B" ,home ,home)))
                 ,@(when (and pass-input-config (> (length (uiop:getenv "XCOMPOSEFILE")) 0)) `(("-R" ,(uiop:getenv "XCOMPOSEFILE") "/_xcomposefile")))
                 ,@ mounts))
               ,@(when verbose-nsjail `("verbose"))
               ,@(when keep-namespaces
                   `(("keep-namespaces" ,keep-namespaces)))
               ,@(when newprivs `("newprivs"))
               ,@(when home `(("home" ,home)))
               ,@(when no-proc `("no-proc"))
               ,@(unless proc-rw `("proc-ro"))
               )))
              ,@(when masking-mounts `(("masking-mounts" ,masking-mounts)))
              ,@(when fake-passwd `("fake-passwd"))
              ,@(when fake-groups `(("fake-groups" ,fake-groups)))
              ,@(when fake-usernames `(("fake-usernames" ,fake-usernames)))
              ,@(when directory `(("directory" ,directory)))
              ,@(when clear-env `("clear-env"))
              ,@(when netns
                  `(("netns"
                     (
                      ,@(when dns `( ((53 :udp) ,@(when (integerp dns) `(() (,dns))))
                                     ((53 :tcp) ,@(when (integerp dns) `(() (,dns))))  ))
                      ,@(when http-proxy `(((,http-proxy tcp))))
                      ,@(when socks-proxy `(((,socks-proxy tcp))))
                      ,@network-ports)
                     ,network-ports-in
                     (
                      ,@(when verbose-netns `("verbose"))
                      ,@(when netns-tuntap-devices
                          `(("tuntap-devices" ,netns-tuntap-devices)))
                      ,@(unless proc-rw `("proc-ro"))
                      ,@(when no-proc `("no-proc"))
                      ))))
              ,@(when pass-stdin `(("stdin-fd" "stdin")))
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
    network-ports keep-namespaces
    bookmarks)
  (declare (ignorable options network-ports))
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
    (when bookmarks
      (uiop:run-program
        (list "rm" "-f" (format nil "~a/places.sqlite" 
                                combined-profile)))
      (let* ((bookmarks
               (loop for b in bookmarks
                     collect
                     (if (listp b) b 
                       (list b b)))))
        (alexandria:write-string-into-file
          (format nil "~{~{<a href=\"~a\">~a</a>~}~%~}"
                  bookmarks)
          (format nil "~a/bookmarks.html" combined-profile)
          :if-exists :supersede :if-does-not-exist :create)))
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
        (unless profile-storage
          (ignore-errors
            (uiop:run-program
              (list
                "rm" "-rf" combined-profile)
              :error-output t)))
        (when marionette-socket
          (ignore-errors
            (uiop:run-program
              (list "rm" "-rf" (directory-namestring marionette-socket))
              :error-output t)))))))
