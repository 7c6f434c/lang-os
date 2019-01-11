(defvar *socket-main-thread* nil)

(defparameter *socket-main-thread-preexisting* *socket-main-thread*)

(setf *random-state* (make-random-state t))

(format t "~s~%Preexisting socket: ~s~%Helpers: ~s~%"
        "Initialising Common Lisp system daemon"
        *socket-main-thread-preexisting*
        *lisp-os-helpers-package*)

(unless
  (ignore-errors
    (with-open-file (*error-output* "/dev/null" :direction :output :if-exists :overwrite)
      (with-open-file (*trace-output* "/dev/null" :direction :output :if-exists :overwrite)
        (load (format nil "~a/lib/common-lisp/lisp-os-helpers/lisp-os-helpers--all-systems.fasl" *lisp-os-helpers-package*))))
    t)
  (load (format nil "~a/lib/common-lisp/lisp-os-helpers/lisp-os-helpers--all-systems.fasl" *lisp-os-helpers-package*)))

(use-package :lisp-os-helpers/shell)
(use-package :lisp-os-helpers/network)
(use-package :lisp-os-helpers/socket-command-server)
(use-package :lisp-os-helpers/subuser)
(use-package :lisp-os-helpers/daemon)
(use-package :lisp-os-helpers/unix-users)
(use-package :lisp-os-helpers/vt)
(use-package :lisp-os-helpers/socket-command-definitions)
(use-package :lisp-os-helpers/auth-data)
(use-package :lisp-os-helpers/kernel)
(use-package :lisp-os-helpers/util)
(use-package :lisp-os-helpers/timestamp)

(unless *socket-main-thread-preexisting*
  (format t "Starting the Common Lisp system daemon at ~a~%" (local-time:now)))

(defvar *vt-spawners* (make-array 12 :initial-element nil))

(loop
  for vtn from 2 to 6 do
  (let ((vtn vtn))
    (unless (elt *vt-spawners* vtn)
      (setf 
	(elt *vt-spawners* vtn)
	(bordeaux-threads:make-thread
	  (lambda ()
	    (loop
	      do (loop while (console-used vtn) do (sleep 10))
	      do (ignore-errors
		   (uiop:run-program
		     (list
		       "/run/current-system/bin/spawn-getty"
		       (format nil "~a" vtn))))
	      ))
	  :name (format nil "Console spawner ~a" vtn))))))

(unless
  (ignore-errors
    (with-open-file (f "/etc/shadow")
      (loop for l := (read-line f nil nil)
            while l
            when (alexandria:starts-with-subseq "root:" l)
            return t)))
  (with-open-file (f "/etc/shadow" :direction :output :if-exists :append)
    (format f "root:!:1::::::~%"))
  (set-password "root" "initial-password-to-change"))

(defparameter
  *socket-main-thread*
  (or *socket-main-thread*
      (bordeaux-threads:make-thread
	(lambda () (eval-socket-runner *system-lisp-socket*))
	:name "System lisp daemon socket command evaluator")))

(unless
  (run-program-return-success
    (uiop:run-program
      (list "env" "NIX_REMOTE=daemon"
            "nix-store" "--check-validity" "/run/current-system/")))
  (system-service "" "nix-daemon"))

(unless
  (run-program-return-success
    (uiop:run-program
      (list "env" "NIX_REMOTE=daemon"
            "nix-store" "--check-validity" "/run/current-system/")))
  (system-service "" "nix-daemon"))

(format
  t "Daemon operations: ~s~%"
  (multiple-value-list
    (ignore-errors
      (ensure-daemon-user "postgres")
      (ensure-daemon-user "named")
      (grant-to-user "named" "/var/lib/bind/")
      (grant-to-group "named" "/var/lib/bind/")
      (ensure-daemon-user "cups")
      (ensure-daemon-group "lp")
      (grant-to-user "cups" "/var/lib/cups/")
      (grant-to-group "lp" "/var/lib/cups/")

      (unless
        (port-open-p 22)
        (system-service "" "from-nixos/openssh"))

      (unless
        (run-program-return-success
          (uiop:run-program
            (list "pgrep" "-x" "udevd")))
        (system-service "" "udevd"))

      (unless
        (port-open-p 631)
        (system-service "" "from-nixos/cups"))

      (unless
        (port-open-p 53)
        (system-service "" "from-nixos/bind"))

      (unless
        (port-open-p 5432)
        (daemon-with-logging 
          "daemon/postgresql"
          (list "su" "postgres" "-s" "/bin/sh" "-c"
                "env -i /run/current-system/services/from-nixos/postgresql")))
      t)))

(defun socket-command-server-commands::run (context &rest command)
  (require-root context)
  (uiop:run-program command :output "/dev/tty62" :error-output "/dev/tty62"))
(defun socket-command-server-commands::run-pipe (context input &rest command)
  (require-root context)
  (with-input-from-string (s input)
    (uiop:run-program command :output :string :input s)))
(defun socket-command-server-commands::quit (context)
  (require-or
    "User presence not confirmed"
    (require-root context)
    (require-presence context))
  (quit))

(defun socket-command-server-commands::restart-udevd (context)
  (require-or
    "User presence not confirmed"
    (require-root context)
    (require-presence context))
  (ignore-errors
    (uiop:run-program
      (list "udevadm" "control" "--exit")
      :error-output t :output t))
  (ignore-errors (uiop:run-program (list "pkill" "udevd")))
  (progn
    (system-service "" "udevd")
    (sleep 0.5)
    (uiop:run-program (list "pgrep" "udev"))
    "OK"))

(defun socket-command-server-commands::restart-openssh (context)
  (require-presence context)
  (if
    (ignore-errors (kill-by-executable "sshd"))
    (progn
      (system-service "" "from-nixos/openssh")
      "OK")
    (error "Stopping openssh failed")))

(defun socket-command-server-commands::restart-cups (context)
  (require-or
    "User presence not confirmed"
    (require-root context)
    (require-presence context))
  (if
    (ignore-errors (kill-by-log "daemon/cups"))
    (progn
      (system-service "" "from-nixos/cups")
      "OK")
    (error "Stopping cupsd failed")))

(defun socket-command-server-commands::restart-bind (context)
  (require-or
    "User presence not confirmed"
    (require-root context)
    (require-presence context))
  (if (ignore-errors (kill-by-log "daemon/bind"))
    (progn
      (system-service "" "from-nixos/bind")
      "OK")
    (error "Stopping bind failed")))

(defun socket-command-server-commands::restart-postgresql (context)
  (require-or
    "Owner user presence not confirmed"
    (require-root context)
    (progn
      (assert (gethash (list (context-uid context) :owner) *user-info*))
      (require-presence context)))
  (if (ignore-errors (kill-by-uid "postgres"))
    (progn
      (daemon-with-logging 
	"daemon/postgresql" 
	(list
	  "su" "postgres" "-s" "/bin/sh" "-c"
	  "env -i /run/current-system/services/from-nixos/postgresql"))
      "OK")
    (error "Stopping postgresql failed")))

(defun socket-command-server-commands::restart-nix-daemon (context)
  (require-or
    "User presence not confirmed"
    (require-root context)
    (require-presence context))
  (if (ignore-errors (kill-by-log "daemon/nix-daemon"))
    (progn
      (system-service "" "nix-daemon")
      "OK")
    (error "Stopping nix-daemon failed")))

(defun socket-command-server-commands::dhclient (context interface &optional copy-resolv)
  (require-or
    "Owner access not confirmed"
    (require-root context)
    (assert (gethash (list (context-uid context) :owner) *user-info*)))
  (require-presence context)
  (uiop:run-program (list "truncate" "--size" "0" "/etc/resolv.conf.dhclient"))
  (uiop:run-program (list "truncate" "--size" "0" "/etc/resolv.conf.dhclient-new"))
  (run-link-dhclient interface)
  (when copy-resolv (dhcp-resolv-conf)))

(defun socket-command-server-commands::add-ip-address (context interface address &optional (netmask-length 24))
  (require-or
    "Owner user presence not confirmed"
    (require-root context)
    (progn
      (assert (gethash (list (context-uid context) :owner) *user-info*))
      (require-presence context)))
  (add-ip-address interface address netmask-length))

(defun socket-command-server-commands::rebuild-from-path
  (context path &optional nix-path)
  (require-presence context)
  (require-root context)
  (uiop:run-program
    `("/run/current-system/bin/update-self-from-expression"
      ,path
      ,@(when nix-path
          (loop
            for path in
            (if
              (stringp nix-path)
              (cl-ppcre:split ":" nix-path) nix-path)
            collect "-I" collect path)))
    :error-output t))

(defun socket-command-server-commands::wpa-supplicant-status (context interface)
  (declare (ignorable context))
  (loop
    for data := (wpa-supplicant-status interface) then (cddr data)
    for key := (first data)
    for value := (second data)
    while data
    collect (list (string-downcase (symbol-name key)) value)))

(defun start-x-allowed-p (context display)
  display
  (require-presence context) t)

(defun grab-device-allowed-p (user subuser device)
  subuser
  (and
    (gethash (list user :owner) *user-info*)
    (or
      (alexandria:starts-with-subseq "/dev/snd/" device)
      (alexandria:starts-with-subseq "/dev/dri/" device)
      (alexandria:starts-with-subseq "/dev/fb" device)
      (alexandria:starts-with-subseq "/dev/video" device)
      (equal "/dev/fuse" device)
      (equal "/dev/kvm" device)
      )))

(defun nsjail-mount-allowed-p (from to type)
  (or
    (equal type "T")
    (equal from to)
    (and
      (or
        (alexandria:starts-with-subseq "/home/" from)
        (alexandria:starts-with-subseq "/tmp/" from)
        )
      (or
        (alexandria:starts-with-subseq "/home/" to)
        (alexandria:starts-with-subseq "/tmp/" to)
        (not (find
               (second (cl-ppcre:split "/" to))
               '(
                 "bin" "boot" "dev" "etc" "home"
                 "initrd" "initramfs"
                 "lost+found" "media"
                 "mnt" "nix" "proc" "root" "run"
                 "sys" "tmp" "usr" "var"
                 "srv" "opt" "net"
                 )
               :test 'equalp))
        ))
    ))

(defun socket-command-server-commands::wifi-modules (context)
  context
  (modprobe "iwlwifi")
  (modprobe "rtl8821ae"))

(defun socket-command-server-commands::reload-video-modules (context)
  (require-or
    "Owner user not confirmed"
    (require-root context)
    (assert (gethash (list (context-uid context) :owner) *user-info*)))
  (loop for m in `("i915" "nouveau" "radeon")
        do (ignore-errors (module-remove m))
        do (modprobe m)))

(defun socket-command-server-commands::ensure-wifi
  (context interface &rest options)
  (require-presence context)
  (when (find "reload-module" options :test 'equalp)
    (module-remove "iwldvm"))
  (unless
    (find-if (lambda (x)
               (alexandria:starts-with-subseq
                 "wlan" (getf x :interface-name)))
             (parsed-ip-address-show))
    (module-remove "iwlwifi"))
  (modprobe "iwlwifi")
  (when (find "restart" options :test 'equalp)
    (stop-wpa-supplicant interface))
  (ensure-wpa-supplicant interface "/root/src/rc/wpa_supplicant.conf")
  (unless
    (wpa-supplicant-wait-connection interface)
    (error "WiFi connection failed on ~a" interface))
  (unless (find "no-dhcp" options :test 'equalp)
    (run-link-dhclient interface)
    (when (find "use-dhcp-resolv-conf" options :test 'equalp)
      (dhcp-resolv-conf)))
  "OK")

(defun socket-command-server-commands::kill-wifi
  (context interface)
  (require-presence context)
  (stop-wpa-supplicant interface)
  (disable-ip-link interface))

(defun socket-command-server-commands::kill-interface
  (context interface)
  (require-presence context)
  (disable-ip-link interface))

(defun socket-command-server-commands::activate-interface
  (context interface)
  (require-presence context)
  (enable-ip-link interface))

(defun socket-command-server-commands::flush-interface
  (context interface)
  (require-presence context)
  (flush-ip-addresses interface))

(defun socket-command-server-commands::local-resolv-conf (context)
  (require-or
    "User presence not confirmed"
    (require-root context)
    (require-presence context))
  (local-resolv-conf)
  "OK")

(setf
  lisp-os-helpers/fbterm-requests:*fbterm-settings*
  (append
    lisp-os-helpers/fbterm-requests:*fbterm-settings*
    `((:font-size ,25))))

(defun socket-command-server-commands::nix-collect-garbage (context)
  (require-or
    "User presence not confirmed"
    (require-root context)
    (require-presence context))
  (uiop:run-program (list "nix-collect-garbage" "-d")))

(defun socket-command-server-commands::nix-cleanup-tests (context)
  (require-or
    "User presence not confirmed"
    (require-root context)
    (require-presence context))
  (uiop:run-program (list "/root/sudo-scripts/nix-cleanup-tests")))

(defun socket-command-server-commands::hostname
  (context hostname)
  (require-or
    "Owner user presence not confirmed"
    (require-root context)
    (progn
      (assert (gethash (list (context-uid context) :owner) *user-info*))
      (require-presence context)))
  (uiop:run-program (list "hostname" hostname)))

(defun socket-command-server-commands::unmount-removable
  (context)
  (require-or
    "User presence not confirmed"
    (require-root context)
    (require-presence context))
  (unmount-removable))

(defun socket-command-server-commands::power-state (context state)
  (require-or
    "User presence not confirmed"
    (require-root context)
    (require-presence context))
  (uiop:run-program (list "wpa_cli" "suspend") :ignore-error-status t)
  (power-state (intern (string-upcase state) :keyword))
  (uiop:run-program (list "wpa_cli" "resume") :ignore-error-status t))

(defun socket-command-server-commands::mount (context device &optional set-user)
  (require-or
    "Owner user presence not confirmed"
    (require-root context)
    (progn
      (assert (gethash (list (context-uid context) :owner) *user-info*))
      (require-presence context)))
  (let*
    ((target (format nil "/media/~a/" (pathname-name device))))
    (ensure-directories-exist target)
    (uiop:run-program
      `("mount" ,device ,target
        ,@(when set-user
            `("-o" ,(format nil "uid=~a" (context-uid context))))))))

(defun socket-command-server-commands::unmount (context device)
  (require-or
    "Owner user presence not confirmed"
    (require-root context)
    (progn
      (assert (gethash (list (context-uid context) :owner) *user-info*))
      (require-presence context)))
  (uiop:run-program (list "umount" device)))

(defun socket-command-server-commands::backup-to (context device)
  (require-or
    "Owner user presence not confirmed"
    (require-root context)
    (assert (gethash (list (context-uid context) :owner) *user-info*)))
  (let*
    ((full-path (format nil "/dev/~a" device))
     (media-path (format nil "/media/~a/" device))
     (backup-path (format nil "~a/backup/" media-path))
     (marker-path (format nil "~a/auto-backup-here" backup-path))
     )
    (cond
      ((not (cl-ppcre:scan "^sd[b-z][1-9]$" device)) "wrong-device-name")
      ((not (ignore-errors (modprobe "uas") (modprobe "usb-storage") t))
       "modprobe-failure")
      ((not (probe-file full-path)) "no-device")
      ((not (prog1
              (ignore-errors
                (and
                  (ignore-errors
                    (ensure-directories-exist media-path)
                    (uiop:run-program 
                      (list "mount" full-path media-path)
                      :error-output
                      (format nil "/tmp/backup-mount-~a.log" device)
                      )
                    t)
                  (probe-file media-path)
                  (probe-file marker-path)
                  (ignore-errors
                    (uiop:run-program
                      (list "/root/script/backup_notebook" backup-path)
                      :output
                      (format nil "/tmp/backup-~a.log" device)
                      :error-output :output)
                    t)
                  ))
              (ignore-errors
                (uiop:run-program (list "umount" full-path)
                                  :output (format nil "/tmp/backup-umount-~a.log" device)
                                  :error-output :output))
              (ignore-errors
                (uiop:run-program (list "umount" media-path)
                                  :output (format nil "/tmp/backup-umount2-~a.log" device)
                                  :error-output :output))
              )) "backup-failed")
      (t "ok"))))

(defun socket-command-server-commands::tether-android (context)
  (require-or
    "Owner user presence not confirmed"
    (require-root context)
    (assert (gethash (list (context-uid context) :owner) *user-info*)))
  (let*
    ((user (context-uid context)))
    (uiop:run-program (list "pkill" "adb") :ignore-error-status t)
    (modprobe "cdc-ether")
    (modprobe "rndis-host")
    (sleep 0.3)
    (grant-acl user "/dev/bus/usb/" :recursive t)
    (ignore-errors (grant-acl user "/sys/bus/usb/devices/" :recursive t))
    (loop
      for subcommand in '(30 31 33) do
      (unless (find "usb0" (parsed-ip-address-show)
                    :test 'equal :key (getf-fun :interface-name))
        (loop
          with inner-command-list :=
          (list "service" "call" "connectivity"
                (format nil "~a" subcommand) "i32" "1")
          for inner-command in
          (list
            (collapse-command inner-command-list)
            (format nil "~{ ~a~}" inner-command-list)
            (format nil "'~{ ~a~}'" inner-command-list))
          do
          (uiop:run-program
            (list "su" user "-c"
                  (collapse-command
                    (list "adb" "shell" "su" "-c" inner-command)))
            :ignore-error-status t))
        (sleep 0.1)))
    (when
      (wait (:timeout 1 :sleep 0.1)
            (find "usb0" (parsed-ip-address-show)
                  :test 'equal :key (getf-fun :interface-name))
            )
      (enable-ip-link "usb0")
      (add-ip-address "usb0" "192.168.42.130"))))

(defun socket-command-server-commands::load-sound (context choice)
  (require-or
    "Owner user presence not confirmed"
    (require-root context)
    (progn
      (assert (gethash (list (context-uid context) :owner) *user-info*))
      (require-presence context)))
  (module-remove-recursive "snd")
  (module-remove-recursive "soundcore")
  (module-remove-recursive "snd-hda-core")
  (module-remove-recursive "snd-hda-intel")
  (module-remove-recursive "snd-usb-audio")
  (modprobe "snd-pcm")
  (modprobe "snd-hda-codec-conexant")
  (cond
    ((equal choice "usb")
     (modprobe "snd-usb-audio")
     (modprobe "snd-hda-intel" "index=1"))
    ((equal choice "none"))
    ((or
       (equal choice "built-in")
       (equal choice "intel")
       (equal choice ""))
      (modprobe "snd-hda-intel")
      (modprobe "snd-usb-audio")))
  (or choice "OK"))

(defun socket-command-server-commands::storage-modules (context)
  (declare (ignorable context))
  (modprobe "uas")
  (modprobe "usb-storage")
  (modprobe "mmc-block"))

(defun socket-command-server-commands::usb-eth-modules (context)
  (declare (ignorable context))
  (modprobe "asix")
  (modprobe "ax88179_178a")
  (modprobe "smsc75xx")
  (modprobe "cdc-ether")
  (modprobe "rndis-host"))

(defun socket-command-server-commands::usb-hid-modules (context)
  (declare (ignorable context))
  (modprobe "usbhid")
  (modprobe "hid_generic")
  (modprobe "hid_cherry"))

(defun socket-command-server-commands::set-cpu-frequency (context frequency)
  (require-or
    "Owner user presence not confirmed"
    (require-root context)
    (progn
      (assert (gethash (list (context-uid context) :owner) *user-info*))
      (require-presence context)))
  (set-cpu-frequency
    (or
      (and (numberp frequency) frequency)
      (ignore-errors (parse-integer frequency))
      (ignore-errors (intern (string-upcase frequency) :keyword))
      frequency)))

(defun socket-command-server-commands::selfcheck-restart (context)
  (require-or
    "Owner user not confirmed"
    (require-root context)
    (assert (gethash (list (context-uid context) :owner) *user-info*)))
  (unless
    (ignore-errors
      (with-open-file (f "/dev/null"))
      (lisp-os-helpers/subuser:subuser-uid "root")
      t)
    (quit)))

(defvar *auto-wifi* nil)
(defvar *auto-interfaces* nil)
(defvar *auto-ip-addresses* nil)
(defvar *auto-modules* nil)
(defvar *auto-acls* nil)

(mapcar 'modprobe *auto-modules*)

(loop for iface in *auto-wifi*
      do (ensure-wpa-supplicant iface "/root/src/rc/wpa_supplicant.conf")
      do (bordeaux-threads:make-thread
	   (let ((iface iface)) 
	     (lambda ()
	       (wpa-supplicant-wait-connection iface)
	       (run-link-dhclient iface)
	       (loop for x in
		     (remove-if-not 
		       (lambda (x) (equal (first x) iface))
		       *auto-ip-addresses*)
		     do (ignore-errors (apply 'add-ip-address x)))))))
(mapcar 'run-link-dhclient *auto-interfaces*)
(loop for iface in *auto-interfaces* do
      (let ((iface iface))
	(bordeaux-threads:make-thread
	  (lambda ()
	    (run-link-dhclient iface)
	    (loop for x in
		  (remove-if-not 
		    (lambda (x) (equal (first x) iface))
		    *auto-ip-addresses*)
		  do (ignore-errors (apply 'add-ip-address x)))))))
(loop for x in *auto-ip-addresses* do
      (ignore-errors (apply 'add-ip-address x)))
(loop for x in *auto-acls* do
      (run-program-return-success
	(uiop:run-program
	  (list "setfacl" "-m" (second x) (first x)))))

(unless
  *socket-main-thread-preexisting*
  (ignore-errors (uiop:run-program (list "/var/current-system/sw/bin/vtlock" "0")))
  (format
    t "Finished Common Lisp daemon initialisation at ~a.~%Socket thread: ~s~%"
    (local-time:now) *socket-main-thread*)
  (bordeaux-threads:join-thread *socket-main-thread*)
  (sleep 5))
