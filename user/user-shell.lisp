(defun load-helpers ()
  (require :sb-posix)
  (require :sb-bsd-sockets)
  (with-open-file (*error-output* "/dev/null" :direction :output :if-exists :overwrite)
    (with-open-file (*trace-output* "/dev/null" :direction :output :if-exists :overwrite)
      (load (format nil "~a/lib/common-lisp/lisp-os-helpers/lisp-os-helpers--all-systems.fasl" *lisp-os-helpers-package*)))))

(unless
  (find-package :lisp-os-helpers/shell)
  (load-helpers))

(defvar *rc-path*
 (or
  (uiop:getenv "USER_LISP_SHELL_RC")
  *compile-file-pathname*
  *load-pathname*))

(use-package :lisp-os-helpers/user-abbreviations)
(use-package :lisp-os-helpers/socket-command-client)
(use-package :lisp-os-helpers/shell)
(use-package :lisp-os-helpers/nix)
(use-package :lisp-os-helpers/subuser-x)
(use-package :lisp-os-helpers/timestamp)
(use-package :lisp-os-helpers/marionette)

(setf *random-state* (make-random-state t))

(defpackage :sudo (:use))

(defun loadrc () (load *rc-path*))
(defun edrc () (editor *rc-path*))

#+sbcl(push 'editor sb-ext:*ed-functions*)

(defmacro defun-export (name args &rest code)
  `(progn
     (defun ,name ,args ,@code)
     (export ',name (find-package ,(package-name (symbol-package name))))))

(defmacro defmacro-export (name args &rest code)
  `(progn
     (defmacro ,name ,args ,@code)
     (export ',name (find-package ,(package-name (symbol-package name))))))

(defun-export
  sudo::start-x (&optional (display 0) command)
  (with-system-socket 
    ()
    (take-reply-value
      (ask-server
        (with-uid-auth 
	  (with-presence-auth
	    "X11 session"
	    `(start-x ,display)))))
    (when command
      (uiop:run-program
	(add-command-env
	  command `((:display ,(format nil ":~a" display))))))))

(defun-export
  sudo::run-as-subuser (name command environment &rest options)
  (with-system-socket
    ()
    (ask-server
      (with-uid-auth
	`(run-as-subuser
	   ,name ,command ,environment ,options)))))

(defun-export
  sudo::restart-system-lisp ()
  (with-system-socket
    ()
    (ignore-errors
      (ask-server
	(with-uid-auth
	  (with-presence-auth
	    "Load fresh code into system lisp"
	    `(quit)))))))

(defun-export
  sudo::system-rebuild
  (&optional
    (path
      (namestring
        (truename
          (format nil "~a/.lang-os-expression.nix" (uiop:getenv "HOME")))))
    (nix-path (uiop:getenv "NIX_PATH")))
  (let*
    ((system-path
       (nix-build "systemInstance"
                  :nix-file path
                  :nix-path (if (stringp nix-path)
                              (cl-ppcre:split ":" nix-path)
                              nix-path))))
    (format t "System: ~s~%" system-path)
    (unless
      (equal
        (truename system-path)
        (truename "/run/current-system/"))
      (with-system-socket
        ()
        (ask-server
          (if
            (equal "root" (get-current-user-name))
            (with-presence-auth
              "Rebuild the system"
              `(rebuild-from-path ,(namestring (truename path)) ,nix-path))
            (with-password-auth
              "Rebuild the system"
              `(rebuild-from-path ,(namestring (truename path)) ,nix-path)
              :user "root")
            ))))))

(defun-export
  sudo::run (&rest command)
  (with-system-socket
    ()
    (ask-server
      (with-password-auth
	"Arbitrary command execution is requested"
	`(run ,@ command)
	:user "root"))))

(defun-export
  sudo::load (path)
  (with-system-socket
    ()
    (ask-server
      (with-password-auth
	"Arbitrary Lisp code load is requested"
	`(load ,path) :user "root"))))

(defun-export
  sudo::eval-string (code)
  (with-system-socket
    ()
    (ask-server
      (with-password-auth
	"Arbitrary Lisp code load is requested"
	`(eval ,code) :user "root"))))

(defmacro
  !su (&rest data)
  `(apply 'sudo:run
     ,(multiple-value-bind
	(command arguments environment)
	(lisp-os-helpers/user-abbreviations::split-command data)
	(assert (null arguments))
	`(add-command-env
	   (list ,@command) (list ,@environment)))))

(defun-export
  sudo::chvt (vtn)
  (with-system-socket
    ()
    (ask-server
      (with-presence-auth
	"Change VT"
	`(chvt ,vtn)))))

(defun-export
  sudo::dhclient (interface &optional copy-resolv)
  (with-system-socket
    ()
    (ask-server
      (with-uid-auth
	(with-presence-auth
	  "Activate network"
	  `(dhclient ,interface ,copy-resolv))))))

(defun-export
  sudo::passwd (&optional password)
  (with-system-socket
    ()
    (ask-server
      (with-password-auth
	"Change the password"
	`(set-password ,password)))))

(defun-export
  sudo::system-shutdown ()
  (with-system-socket
    ()
    (ask-server
      (with-presence-auth
	"Shutdown the system: go to ramfs console"
	`(system-shutdown)))))

(defun-export
  sudo::system-poweroff ()
  (with-system-socket
    ()
    (ask-server
      (with-presence-auth
	"Shutdown the system: poweroff"
	`(system-poweroff)))))

(defun-export
  sudo::system-reboot ()
  (with-system-socket
    ()
    (ask-server
      (with-presence-auth
	"Shutdown the system: reboot"
	`(system-reboot)))))

(defun-export
  sudo::grab-devices (devices &optional subuser)
  (with-system-socket
    ()
    (ask-server
      (with-uid-auth
        `(grab-devices
           ,(mapcar
              'namestring
              (reduce
                'append
                (mapcar
                  'directory
                  devices)))
           ,subuser)))))

(defun-export
  sudo::set-brightness (brightness)
  (with-system-socket
    ()
    (ask-server
      (with-presence-auth
        "Set screen brightness"
        `(set-brightness ,brightness)))))

(defun update-firefox-launcher ()
  (reset-firefox-launcher :nix-path (list ($ :home) "/home/repos")
                          :nix-wrapper-file (format nil "~a/src/nix/lang-os/wrapped-firefox-launcher.nix"
                                                    ($ :home))
                          :profile-contents
                          (format nil "~a/src/nix/lang-os/user/firefox-profile-skel/"
                                  ($ :home))))

(unless lisp-os-helpers/subuser-x:*firefox-launcher* (update-firefox-launcher))

(defun firefox (ff-args &rest args &key
                        (pass-stderr t) (pass-stdout t)
                        (no-netns nil no-netns-p)
                        (netns t) mounts prefs
                        (socks-proxy nil)
                        (http-proxy (unless socks-proxy 3128))
                        (dns t) environment network-ports file
                        (marionette-socket (unless (or no-netns (not netns)) t))
                        data data-ro
                        (certificate-overrides
                          (uiop:getenv "FIREFOX_CERTIFICATE_OVERRIDES"))
                        &allow-other-keys)
  (apply
    'subuser-firefox
    `(,@ff-args 
       ,@(when file
           `(,(format
                nil "file:///~a"
                (namestring (truename file))))))
    :pass-stderr pass-stderr
    :pass-stdout pass-stdout
    :certificate-overrides certificate-overrides
    :prefs prefs
    :environment environment
    :dns dns
    :http-proxy http-proxy
    :socks-proxy socks-proxy
    :network-ports network-ports
    :mounts
    `(
      ,@ mounts
      ,@ (when file `(("-B" ,file)))
      ,@ (when data `(("-B" ,data "/data")))
      ,@ (when data-ro `(("-R" ,data-ro "/data-ro")))
      )
    :marionette-socket marionette-socket
    :allow-other-keys t
    (append
      (when no-netns-p `(:netns nil))
      args)))

(defun-export
  sudo::root-urxvt
  (&key (display 0)
        (command
          (list
            "my-screen"
            (format nil "for-su-from-~a" (get-current-user-name))
            "bash")))
  (&&
    (sudo:run
      "su" "root" "-l" "-c"
      (collapse-command
        `("env" "--" 
          ,(format nil "DISPLAY=:~a" display)
          "MY_SCREEN_TITLE=su screen"
          ,(which "urxvt") "-e" ,@ command)))))

(defun-export
  sudo::wifi (interface &optional (dhclient t))
  (with-system-socket
    ()
    (ask-server
      (with-presence-auth
        "Connect to WiFi"
        `(ensure-wifi ,interface ,(unless dhclient "no-dhcp"))))))

(defmacro ask-with-auth ((&key presence root password) &rest code)
  `(with-system-socket
     ()
     (ask-server
       (with-uid-auth
         (,@(if presence `(with-presence-auth ,presence) `(identity))
           (,@(if password `(with-password-auth ,password) `(identity))
             (,@(if root `(with-password-auth ,root) `(identity))
               (list 'list ,@code)
               ,@(if root `(:user "root")))))))))

(defun restart-lisp-shell-server (&key rebuild)
 (when rebuild
   (! lisp-shell-build :< nil :&> nil)
   (format t "Image rebuilt~%"))
 (! screen "-S" lisp-shell-socket-evaluator "-X" quit)
 (sleep 0.5)
 (! lisp-shell-server))

(defun full-refresh ()
  (sudo:system-rebuild)
  (sudo:restart-system-lisp)
  (restart-lisp-shell-server :rebuild t)
  (load-helpers)
  (loadrc)
  (update-firefox-launcher))

(defun-export
  sudo::add-ip-address (interface address &optional netmask-length)
  (ask-with-auth
    (:presence "Add a network address")
    `(add-ip-address ,interface ,address ,(or netmask-length 24))))

(defun-export
  sudo::hostname (hostname)
  (ask-with-auth
    (:presence "Set the machine hostname")
    `(hostname ,hostname)))

(defun-export
  sudo::rewifi (&key (interface "wlan0") restart (dhcp t) dhcp-resolv-conf)
  (ask-with-auth
    (:presence "Reconnect to WiFi")
    `(ensure-wifi ,interface ,(when restart "restart") ,(unless dhcp "no-dhcp")
                  ,(when dhcp-resolv-conf "use-dhcp-resolv-conf"))
    `(restart-bind))
  (! proxy-restart (format nil "~a/src/rc/squid/direct.squid" ($ :home))))

(defun ~ (&rest components)
  (format nil "~a~{/~a~}"
          ($ :home) components))

(defun
  enter-master-password ()
  (!! enter-master-password)
  (wait (:timeout 300 :sleep 1)
        (probe-file
          (~ "queries/p-store/zsh@consistency")))
  (alexandria:read-file-into-string
    (~ "queries/p-store/zsh@consistency")))

(defmacro
  email-fetcher (suffix period)
  `(progn
     (overwrite-file
       (format nil "~a" ,period)
       (format nil "~a/.email-updater-sleep~a"
               ($ :home) ,(or suffix "")))
     (! := (:fdm_suffix ,(or suffix "")) email-updater-starter detach)))

(defun wpa-status (&optional (interface "wlan0"))
  (let*
    ((data
       (ignore-errors
         (first
           (take-reply-value
             (ask-with-auth
               ()
               `(wpa-supplicant-status ,interface))))))
     (ssid (second (find "ssid" data :test 'equalp :key 'first)))
     (state (second (find "wpa_state" data :test 'equalp :key 'first))))
    (cond
      ((null ssid) "")
      ((equalp state "completed") (format nil "~a*" ssid))
      (t (format nil "~a*~a" ssid state)))))

(defun email-fetchers-fast ()
  (email-fetcher "-main" 20)
  (email-fetcher "" 60)
  (email-fetcher "-qip" 300))

(defun email-fetchers-slow ()
  (email-fetcher "-main" 60)
  (email-fetcher "" 300)
  (email-fetcher "-qip" 1200))

(defun email-fetchers-ultraslow ()
  (email-fetcher "-main" 900)
  (email-fetcher "" 1800)
  (email-fetcher "-qip" 7200))

(defun commit-rc ()
  (let*
    ((cwd (uiop:getcwd)))
    (cd (concatenate 'string ($ :home) "/src/rc"))
    (! mtn ci -m "Event flow"           remind-calendar-input)
    (! mtn ci -m "SSL in the wild"      ssl-exceptions)
    (! mtn ci -m "Firefox prefs tuning" firefox-prefs.js)
    (cd cwd)))

(defun im-online-here (&key skip-ii skip-mcabber)
  (! := (:skip_ii_launch (or skip-ii ""))
     := (:skip_mcabber_launch (or skip-mcabber ""))
     im-online-here))

(defmacro system-eval (&rest code)
  (let*
    ((code-string (format nil "(progn ~{ ~s ~})" code)))
    `(ask-with-auth (:root ,(not (equal (get-current-user-name) "root")))
                    `(eval ,,code-string))))

(defun grab-sound (&optional name)
  (sudo:grab-devices `("/dev/snd/*") name))

(defun grab-video (&optional name)
  (sudo:grab-devices `("/dev/video*") name))

(defun grab-for-videochat (&optional name)
  (sudo:grab-devices `("/dev/video*" "/dev/snd/*") name))

(defun grab-kvm (&optional name)
  (sudo:grab-devices `("/dev/kvm") name))

(defun grab-fuse (&optional name)
  (sudo:grab-devices `("/dev/fuse") name))

(defun-export 
  sudo::load-sound (choice)
  (ask-with-auth (:presence "Load sound configuration")
                 `(load-sound ,choice)))

(defun grab-default-devices ()
  (sudo:load-sound "usb")
  (grab-sound)
  (grab-kvm)
  (grab-fuse))

(defmacro define-scoped-shell-command
  (name (command-var &key
                     skip-command-var arguments
                     default-command
                     (runner `(append
                                (list "screen" "-X" "screen")
                                ,command-var)))
        before after)
  (let*
    ((fifo (gensym))
     (thread (gensym)))
    `(defun ,name (,@(unless skip-command-var (list command-var))
                    ,@arguments)
       (let*
         ((,fifo (make-temporary-fifo))
          (,thread (bordeaux-threads:make-thread 
                     (lambda () (wait-on-fifo ,fifo))
                     :name "FIFO wait thread for a scoped command"))
          (,command-var (add-command-fifo-fd
                          (or ,command-var ,default-command)
                          ,fifo)))
         ,before
         (unless
           (run-program-return-success
             (uiop:run-program ,runner))
           (bordeaux-threads:destroy-thread ,thread))
         (ignore-errors (bordeaux-threads:join-thread ,thread))
         ,after))))

(defun rootp () (equal "root" (get-current-user-name)))
(defun non-root-p () (not (rootp)))

(define-scoped-shell-command
  shell-with-mounted-devices (command
                               :default-command "$SHELL"
                               :arguments (&rest devices))
  (ask-with-auth
    (:presence (non-root-p))
    (cons
      "list"
      (loop for device in devices
            collect `(mount ,@(if (stringp device) (list device) device)))))
  (ask-with-auth
    (:presence (non-root-p))
    (cons
      "list"
      (loop for device in devices
            collect `(unmount ,@(if (stringp device) (list device) device))))))
