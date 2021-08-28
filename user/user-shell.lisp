; vim:filetype=lisp
(defun load-helpers ()
  (require :sb-posix)
  (require :sb-bsd-sockets)
  (with-open-file (*error-output* "/dev/null" :direction :output :if-exists :overwrite)
    (with-open-file (*trace-output* "/dev/null" :direction :output :if-exists :overwrite)
      (ignore-errors
        (load (format nil "~a/lib/common-lisp/lisp-os-helpers/lisp-os-helpers--all-systems.fasl" *lisp-os-helpers-package*))))))

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
(use-package :lisp-os-helpers/network)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'lisp-os-helpers/subuser-x::*firefox-profile-contents*)
  (import 'lisp-os-helpers/subuser-x::*firefox-profile-combiner*))

(setf *random-state* (make-random-state t))

(defpackage :sudo (:use))

(defun loadrc () (load *rc-path*))
(defun edrc () (editor *rc-path*))

#+sbcl(push 'editor sb-ext:*ed-functions*)

(defun-export
  sudo::start-x (&optional (display 0) command)
  (with-system-socket 
    ()
    (take-reply-value
      (ask-server
        (with-uid-auth 
	  (with-presence-auth
	    "X11 session"
	    `(list
               ,@(unless (probe-file "/dev/dri/card1")
                   `((reload-video-modules)))
               (restart-udevd)
               (start-x ,display))))))
    (when command
      (uiop:run-program
	(add-command-env
	  command `((:display ,(format nil ":~a" display))
                    (:ld_library_path "/run/opengl-driver/lib")))))))

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
    (format *trace-output* "System: ~s~%" system-path)
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
  `(apply 'sudo::run
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
	  `(progn
             (dhclient ,interface ,copy-resolv)
             ,@(unless copy-resolv `(local-resolv-conf))))))))

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
  sudo::ungrab-devices (devices &optional subuser)
  (with-system-socket
    ()
    (ask-server
      (with-uid-auth
        `(ungrab-devices
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

(defun-export
  sudo::set-cpu-frequency (freq)
  (with-system-socket
    ()
    (ask-server
      (with-uid-auth
        (with-presence-auth
          "Set CPU frequency"
          `(set-cpu-frequency ,freq))))))

(defun-export
  sudo::console-keymap (&key (keymap "local/ru-en.map"))
  (ask-with-auth (:presence t)
                 `(console-keymap ,@(when keymap (list keymap)))))

(defvar *firefox-variants* (make-hash-table :test 'equal))

(defmacro with-firefox-launcher ((profile combiner launcher) &body body)
  `(let* ((*firefox-profile-contents* ,profile)
          (*firefox-profile-combiner* ,combiner)
          (*firefox-launcher* ,launcher))
     (declare (special *firefox-profile-contents*)
              (special *firefox-profile-combiner*)
              (special *firefox-launcher*))
     ,@body))

(defmacro with-firefox-launcher-packed ((pack) &body body)
  (let ((p (gensym)))
    `(let ((,p ,pack))
       (with-firefox-launcher ((first ,p) (second ,p) (third ,p)) ,@body))))

(defmacro with-firefox-variant ((&optional variant) &body body)
  `(with-firefox-launcher-packed 
     ((gethash ,(string-downcase variant) *firefox-variants*)) ,@body))

(defun update-firefox-launcher (&key variant fast) 
  (let ((variant-string (if variant (string-downcase (format nil "-~a" variant)) "")))
    (with-firefox-launcher
      (nil nil nil)
      (reset-firefox-launcher
        :nix-path (append (cl-ppcre:split ":" ($ :nix_path)) (list ($ :home) "/home/repos"))
        :nix-wrapper-file (format nil "~a/src/nix/lang-os/wrapped-firefox-launcher.nix"
                                  ($ :home))
        :out-link (~ ".nix-personal" (concatenate 'string "firefox-launcher" variant-string))
        :fast fast
        :profile-contents
        (format nil "~a/src/nix/lang-os/user/firefox-profile-skel~a/"
                ($ :home)
                variant-string)
        :verbose t)
      (setf (gethash (string-downcase variant) *firefox-variants*)
            (list *firefox-profile-contents* *firefox-profile-combiner* *firefox-launcher*))
      (reset-bus-helpers
        :nix-path (append (cl-ppcre:split ":" ($ :nix_path)) (list ($ :home) "/home/repos"))
        :nix-file (~ "src/nix/lang-os/bus-wrappers.nix")
        :out-link (~ ".nix-personal/bus-wrapper")
        :fast fast))
    (unless variant
      (let ((pack (gethash nil *firefox-variants*)))
        (setf *firefox-profile-contents* (first pack)
              *firefox-profile-combiner* (second pack)
              *firefox-launcher* (third pack))))))

(defun update-firefox-variants (&key fast)
  (update-firefox-launcher :fast fast)
  (update-firefox-launcher :variant 'natural :fast fast)
  (update-firefox-launcher :variant 'noconfig :fast fast))

(unless lisp-os-helpers/subuser-x:*firefox-launcher*
  (when (probe-file (~ "src/nix/lang-os"))
    (update-firefox-variants :fast t)))

(defun ethernet-attached (interface)
  (getf (first (lisp-os-helpers/network::parsed-ip-address-show interface)) :lower-up))

(defun marionette-remove-hotkey-request (key modifier)
  `(
    (progn
      (defun find-hotkey (key modifiers)
        (ps:chain
          document document-element
          (query-selector
            (+ "keyset key[key='" key "'][modifiers='" modifiers "']"))))
      (let*
        ((x (find-hotkey ,key ,modifier)))
        (ps:chain x parent-node (remove-child x))))))

(defun stumpwm-eval (form &key as-string to-string)
  (let*
    ((socket
       (iolib:make-socket
         :address-family :local :type :stream
         :connect :active
         :remote-filename
         (format 
           nil "~a/.stumpwm-socket/stumpwm-socket/socket"
           ($ :home)))))
    (unwind-protect
      (progn
        (when to-string (format socket "(format nil \"~~a\"~%"))
        (format socket (if as-string "~a~%" "~s~%") form)
        (when to-string (format socket ")~%"))
        (finish-output socket)
        (read socket))
      (ignore-errors (close socket)))))

(defun stumpwm-app-tagger (tags &key (keep t) forever
                                (wait t) (now nil))
  (lambda (&key options)
    (let*
      ((hostname
         (second
           (find
             "hostname"
             (cdr (find "nsjail" options
                        :key 'first :test 'equalp))
             :key 'first :test 'equalp))))
      (when wait
        (stumpwm-eval
          `(wait-set-tags-by-hostname
             ,hostname
             ',tags :keep ,keep :forever ,forever) :to-string t))
      (when now
        (stumpwm-eval
          `(set-tags-by-hostname
             ,hostname
             ',tags :keep ,keep) :to-string t)))))

(defun firefox (ff-args &rest args &key
                        (pass-stderr t) (pass-stdout t)
                        (no-netns nil no-netns-p)
                        (netns t) mounts prefs
                        (socks-proxy nil)
                        (http-proxy (unless socks-proxy 3128))
                        (dns t) environment network-ports file
                        (marionette-socket (unless (or no-netns (not netns)) t))
                        data data-ro name javascript autorefresh
                        (certificate-overrides
                          (uiop:getenv "FIREFOX_CERTIFICATE_OVERRIDES"))
                        marionette-requests marionette-requests-quiet
                        no-close after-marionette-requests
                        marionette-requests-wait-content
                        extensions late-urls
                        stumpwm-tags
                        hostname-hidden-suffix hostname-suffix
                        grab-sound grab-camera grab-devices
                        extra-urls keep-namespaces
                        &allow-other-keys)
  (let*
    ((name (or name (timestamp-usec-recent-base36)))
     (marionette-socket
       (cond
         ((eq t marionette-socket)
          (getf (subuser-name-and-marionette-socket :name name)
                :marionette-socket))
         (t marionette-socket)))
     (marionette-requests
       (append
         (when no-close
           (list
             (marionette-remove-hotkey-request "Q" "accel")
             (marionette-remove-hotkey-request "W" "accel")
             ))
         (when extra-urls
           (loop for u in (cl-ppcre:split "[|]" extra-urls)
                 collect
                 `((ps:chain
                     g-browser
                     (add-tab ,u (ps:create
                                   'triggering-principal "Marionette"))))))
         marionette-requests))
     (arglist 
       (append
         (list         
           :pass-stderr pass-stderr
           :pass-stdout pass-stdout
           :certificate-overrides certificate-overrides
           :prefs `(
                    ,(if javascript 
                        `("javascript.enabled" t)
                        `("javascript.enabled" nil))
                    ,@(when autorefresh
                        `(("accessibility.blockautorefresh" nil)))
                    ,@prefs )
           :environment environment
           :dns dns
           :http-proxy http-proxy
           :socks-proxy socks-proxy
           :network-ports network-ports
           :mounts
           `(
             ,@ mounts
             ,@ (when file `(("-B" ,file)))
             ,@ (when data `(("-B" ,data "/_data")))
             ,@ (when data-ro `(("-R" ,data-ro "/_data-ro")))
             )
           :marionette-socket marionette-socket
           :name name
           :grab-devices
           `(
             ,@ (when grab-sound (list "/dev/snd*"))
             ,@ (when grab-camera (list "/dev/video*"))
             ,@ grab-devices
             )
           :keep-namespaces keep-namespaces
           :allow-other-keys t)
         (when no-netns-p `(:netns nil))
         args)))
    (when
      (and 
        (or marionette-requests
            after-marionette-requests
            stumpwm-tags extensions late-urls)
        marionette-socket)
      (bordeaux-threads:make-thread
        (lambda ()
          (when
            (wait (:timeout 30 :sleep 0.3)
                  (ignore-errors
                    (and
                      (probe-file marionette-socket)
                      (with-marionette
                        (marionette-socket)
                        (ask-marionette-parenscript '(ps-js:return "1"))))))
            (format *trace-output* "Marionette socket ~s responds~%"
                    marionette-socket)
            (with-marionette
              (marionette-socket)
              (marionette-wait-ready :context :chrome :timeout 15)
              (loop for e in extensions
                    do (format *trace-output* "Installing ~s~%" e)
                    do (format *trace-output*
                               "Result: ~a~%"
                               (ignore-errors 
                                 (ask-marionette
                                   (format nil "Addons(session).install(~s,True),session.set_context(session.CONTEXT_CONTENT)" e)))))
              (loop for u in late-urls
                    do (format 
                         *trace-output*
                         "Opening URL: ~s~%Result: ~a~%"
                         u
                         (ignore-errors
                           (ask-marionette
                             (format 
                               nil 
                               "session.switch_to_window(session.open('tab',True)['handle'],True), session.set_context(session.CONTEXT_CONTENT), session.navigate('~a')" (lisp-os-helpers/marionette::escape-for-python u))))))
              (when marionette-requests-wait-content
                (marionette-wait-ready 
                  :context :content :timeout
                  (if (eq marionette-requests-wait-content t) 15
                    marionette-requests-wait-content)))
              (loop
                for r in marionette-requests
                do (if marionette-requests-quiet
                     (ignore-errors (apply 'ask-marionette-parenscript r))
                     (format
                       *trace-output*
                       "Request~%~s~%translated as ~%~s~% gave response ~%~s~%"
                       r (ps:ps* (first r))
                       (ignore-errors (apply 'ask-marionette-parenscript r))))))
            (format *trace-output* "Done feeding Marionette~%")
            (when after-marionette-requests
              (apply 
                after-marionette-requests
                :allow-other-keys t
                arglist))
            (when stumpwm-tags
              (funcall
                (stumpwm-app-tagger
                  (if (stringp stumpwm-tags)
                    (cl-ppcre:split "[| ]" stumpwm-tags)
                    stumpwm-tags)
                  :keep nil :wait nil :now t)
                :options
                `(("nsjail" 
                   ("hostname" 
                    ,(format nil
                             "~a~a~a"
                             (masked-username
                               name hostname-hidden-suffix)
                             (if hostname-suffix "." "")
                             (or hostname-suffix ""))))))
              )))
          :name "Marionette command feeder"))
    (apply 'subuser-firefox
           `(,@ff-args 
              ,@(when file
                  `(,(format
                       nil "file:///~a"
                       (namestring (truename file))))))
           arglist)))

(defun-export
  sudo::root-urxvt
  (&key (display 0)
        (command
          (list
            (true-executable "my-screen")
            (format nil "for-su-from-~a" (get-current-user-name))
            "bash")))
  (&&
    (sudo::run
      "su" "root" "-l" "-c"
      (collapse-command
        `("env" "--" 
          ,(format nil "DISPLAY=:~a" display)
          "MY_SCREEN_TITLE=su screen"
          ,(true-executable "urxvt") "-e" ,@ command)))))

(defun-export
  sudo::wifi (interface &optional (dhclient t))
  (with-system-socket
    ()
    (ask-server
      (with-presence-auth
        "Connect to WiFi"
        `(ensure-wifi ,interface ,(unless dhclient "no-dhcp"))))))

(defun restart-lisp-shell-server (&key rebuild)
 (when rebuild
   (! lisp-shell-build :< nil :&> nil)
   (format *trace-output* "Image rebuilt~%"))
 (! screen "-S" lisp-shell-socket-evaluator "-X" quit)
 (sleep 0.5)
 (! lisp-shell-server))

(defun system-build (&rest args &key
                           (nix-path (uiop:getenv "NIX_PATH"))
                           (nix-file (~ ".lang-os-expression.nix"))
                           &allow-other-keys)
  (apply 'nix-build "systemInstance"
         (append
           args
           (list
             :nix-file nix-file
             :nix-path (if (stringp nix-path)
                         (cl-ppcre:split ":" nix-path)
                         nix-path)))))

(defun full-refresh ()
  (sudo::system-rebuild)
  (sudo::restart-system-lisp)
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
  sudo::rewifi (&key (interface "wlan0") restart (dhcp t) dhcp-resolv-conf
                     reload-module no-dns-forward)
  (ask-with-auth
    (:presence "Reconnect to WiFi")
    (if (ethernet-attached "eth0") `(progn) `(flush-interface "eth0"))
    (if dhcp-resolv-conf `(progn) `(local-resolv-conf))
    `(ensure-wifi ,interface ,(when restart "restart") ,(unless dhcp "no-dhcp")
                  ,(when dhcp-resolv-conf "use-dhcp-resolv-conf")
                  ,(when reload-module "reload-module"))
    `(reconfigure-bind "restart" ,@(when no-dns-forward `("empty"))))
  (! proxy-restart (format nil "~a/src/rc/squid/direct.squid" ($ :home))))

(defun grab-fuse (&optional name)
  (sudo::grab-devices `("/dev/fuse") name))

(defun
  enter-master-password ()
  (grab-fuse)
  (! queryfs-session-run detach)
  (! enter-master-password
     :< (list (first 
                (take-reply-value
                  (ask-with-auth
                    ()
                    `(request-secret "Enter the master password" 15))))
              :terpri t))
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
  (sudo::grab-devices `("/dev/snd/*") name))

(defun grab-video (&optional name)
  (sudo::grab-devices `("/dev/video*") name))

(defun grab-for-videochat (&optional name)
  (sudo::grab-devices `("/dev/video*" "/dev/snd/*") name))

(defun grab-kvm (&optional name)
  (sudo::grab-devices `("/dev/kvm") name))

(defun-export 
  sudo::load-sound (choice)
  (ask-with-auth (:presence "Load sound configuration")
                 `(load-sound ,choice)))

(defun grab-dri (&optional name)
  (sudo::grab-devices `("/dev/dri/card*" "/dev/dri/render*") name))

(defun grab-default-devices ()
  (sudo::load-sound "usb")
  (grab-kvm)
  (grab-fuse)
  (sleep 1)
  (grab-sound)
  (grab-dri))

(defmacro define-scoped-command
  (name arguments (before after) &rest code)
  `(defun ,name ,arguments
     (unwind-protect
       (progn ,before ,@code)
       ,after)))

(defun rootp () (equal "root" (get-current-user-name)))
(defun non-root-p () (not (rootp)))

(define-scoped-command
  shell-with-mounted-devices (command &rest devices)
  (
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
             collect `(unmount ,@(if (stringp device) (list device) (list (first device)))))))
   )
  (wait-on-shell-command (or command "$SHELL")))

(defun-export
  sudo::tether-android ()
  (ask-with-auth
    ()
    `(tether-android)))

(defun-export
  sudo::backup-to (target)
  (ask-with-auth
    ()
    `(backup-to ,target)))

(defun standby
  (&key (state "mem") (sync t) (forget-secrets t) im-offline (re-wifi t) (randr t) kill-my-x x-lock)
  (loop for d in (directory (~ "mnt/*/")) do
        (& fusermount -u (namestring d)))
  (when randr (! := (:display (or ($ :display) ":0")) x-randr-options))
  (when x-lock (! := (:display (or ($ :display) ":0")) xscreensaver-command -lock))
  (! sh -c "echo 5 > ~/.watchperiod")
  (when sync (& sync))
  (! web-stream-updater-starter quit)
  (when im-offline (! im-offline))
  (when forget-secrets (! forget-secrets))
  (when kill-my-x (! pkill "Xorg"))
  (when sync (! sync))
  (ignore-errors (ask-with-auth (:presence t) `(power-state ,state)))
  (sleep 5)
  (when randr (& := (:display (or ($ :display) ":0")) x-randr-options))
  (when re-wifi (sudo::rewifi)))

(defun boot-login-init (&key (wifi "wlan0") (sound "usb"))
  (ask-with-auth (:presence t)
                 `(list
                    ,(if wifi
                       `(background-thread
                          (ensure-wifi
                            ,(if (stringp wifi) wifi "wlan0")))
                       `(list))
                    (load-sound ,sound)
                    (storage-modules) (usb-hid-modules)
                    (usb-eth-modules) (laptop-power-modules)
                    (hostname "localhost")
                    (set-brightness 50) (set-cpu-frequency 2690)
                    (console-keymap "local/ru-en.map")
                    ))
  (! queryfs-session-run detach)
  (& cleanup-loops)
  (grab-kvm) (grab-fuse)
  (sudo::grab-devices `("/dev/fb*"))
  (restart-lisp-shell-server))

(defun gvim-plus-zathura (file &key compiler)
  (with-open-file (f file :if-does-not-exist :create))
  (let*
    ((fullname (namestring (truename file)))
     (directory (directory-namestring fullname))
     (pdfname (cl-ppcre:regex-replace "([.][^.]*)?$" fullname ".pdf")))
    (when compiler
      (uiop:run-program
        `("env" "-C" ,directory "--" ,@ compiler ,fullname)
        :output t :error-output t))
    (&& (subuser-nsjail-x-application
          (list (true-executable "zathura") pdfname)
          :mounts `(("-R" ,directory))
          :pass-stderr t :grab-dri t))
    (& gvim (or fullname) :error-output *error-output*)))

(defun start-stumpwm (display)
  (sudo::start-x display "x-options & x-daemons & synsettings & keymap-more-symbols; sleep 5; zsh -c stumpwm && sleep 3"))

(defun start-in-stumpwm (display command)
  (sudo::start-x 
    display
    (concatenate 'string
                 "x-options & x-daemons & synsettings & keymap-more-symbols; stumpwm &"
                 command)))

(defun start-ratpoison (display)
  (sudo::start-x display "x-options & synsettings & keymap-more-symbols & ratpoison& sleep 1; ratpoison-config"))

(defun start-in-ratpoison (display command)
  (sudo::start-x
    display
    (concatenate 'string
                 "ratpoison & sleep 1; ratpoison-config & keymap-more-symbols & "
                 command)))

(defun enter-location (&key ii (mcabber t) (brightness 25) (freq 2690)
                        (interface "wlan0") (extra-ips `())
                        (watchperiod "0.3") (location "somewhere")
                        (extra-requests ()) (skip-wifi nil)
                        (bind-forward nil) (restart-bind (not skip-wifi))
                        (proxy-config "direct.squid"))
  (alexandria:write-string-into-file
    location
    (format nil "~a/.location" (uiop:getenv "HOME"))
    :if-exists :supersede)
  (ask-with-auth
    (:presence t)
    (if skip-wifi `(progn) `(ensure-wifi ,interface))
    (if restart-bind
      `(reconfigure-bind "restart" ,@(unless bind-forward `("empty")))
      `(progn))
    `(set-brightness ,brightness)
    `(set-cpu-frequency ,freq)
    `(list
       ,@(loop for ip in extra-ips collect
               `(add-ip-address ,interface ,ip)))
    `(progn ,@ extra-requests))
  (when proxy-config
    (& proxy-restart (format nil "~a/src/rc/squid/~a" ($ :home) proxy-config)))
  (enter-master-password)
  (email-fetchers-fast)
  (im-online-here
    :skip-ii (not ii)
    :skip-mcabber (not mcabber))
  (& x-daemons)
  (alexandria:write-string-into-file
    (format nil "~a" watchperiod)
    (format nil "~a/.watchperiod" (uiop:getenv "HOME"))
    :if-exists :supersede)
  )

(defun enter-home (&rest args &key (extra-ips `())
                         (location "home")
                         &allow-other-keys)
  (apply
    'enter-location
    :location location :extra-ips extra-ips
    args))

(defun launch-process-and-tag-windows (command tags &key keep forever launch-parameters)
  (let*
    ((process (apply 'uiop:launch-program command launch-parameters))
     (pid (uiop:process-info-pid process)))
    (stumpwm-eval `(wait-set-tags-by-pid
                     ,pid ',tags :keep ,keep :forever ,forever)
                  :to-string t)))

(defun add-cffi-libs-from-nix (package &key (suffix "lib") 
                                  (nix-path (uiop:getenv "NIX_PATH"))
                                  (nix-file "<nixpkgs>"))
  (let* ((nix-path (if (stringp nix-path) (cl-ppcre:split ":" nix-path) nix-path))
         (package-path (nix-build package :nix-file nix-file
                                  :nix-path nix-path :nix-realise-error-output t))
         (library-path (format nil "~a/~a" package-path suffix)))
    (push (truename library-path) cffi:*foreign-library-directories*)))

(defun slay-stale-subuser-socats ()
  (let* ((ht (make-hash-table :test 'equal)))
    (loop for line in (ps)
          for uid-s := (getf line :uid)
          for pid-s := (getf line :pid)
          for uid := (or (ignore-errors (parse-integer uid-s)) -1)
          for command := (getf line :command)
          for large := (> uid 100000)
          for socat-p := (cl-ppcre:scan "^socat " command)
          for firefox-p := (cl-ppcre:scan "^(/bin/sh )?([-/a-z0-9A-Z_.]*/)?firefox(-launcher)? " command)
          for status := (gethash uid ht)
          when (and large (not (equal status t))) do
          (setf (gethash uid ht)
                (cond (firefox-p t)
                      (socat-p (cons pid-s status))
                      (t status))))
    (maphash (lambda (uid pids)
               (format t "~s~%" (list uid pids))
               (when (and pids (listp pids))
                 (let* ((name (ask-with-auth () `(select-subuser-by-uid ,uid))))
                   (format t "~s~%" (list uid name))
                   (when name
                     (sudo::run-as-subuser name `("kill" ,@pids) ()
                                           "wait")))))
             ht)))

(defun alive-users ()
  (loop
    with ht := (make-hash-table :test 'equal)
    with res := nil
    for x in (ps)
    do (setf (gethash (getf x :uid) ht) t)
    finally (progn
              (maphash (lambda (k v) v (push k res)) ht)
              (return res))))

(defun acl-users (filename)
  (loop for line in ($ () getfacl -n (identity filename))
        for components := (cl-ppcre:split ":" line)
        for kind := (first components)
        for id := (second components)
        when (equal kind "user")
        when (> (length id) 0)
        collect id))

(defun subuser-suffixes (uids)
  (loop 
    for name in 
    (first
      (take-reply-value
        (ask-with-auth 
          ()
          `(list 
             ,@(loop for u in uids
                     collect `(ignore-errors
                                (select-subuser-by-uid
                                  ,(parse-integer (format nil "~a" u)))))))))
    for suffix :=
    (and name
         (> (length name) (length (get-current-user-name)))
         (equal (format nil "~a." (get-current-user-name))
                (subseq name 0 (1+ (length (get-current-user-name)))))
         (subseq name (1+ (length (get-current-user-name)))))
    when suffix
    collect suffix))

(defun atmost (l n)
  (subseq l 0 (min (length l) n)))

(defun ungrab-for-stale (devices n)
  (let*
    ((devices (mapcar 'namestring (reduce 'append (mapcar 'directory devices))))
     (acl-users (loop with res := nil
                      for d in devices
                      do (setf res (union res (acl-users d) :test 'equal))
                      finally (return res)))
     (alive-users (alive-users))
     (users (set-difference acl-users alive-users :test 'equal))
     (to-ungrab (atmost users n))
     (suffixes (subuser-suffixes to-ungrab)))
    (format t "~a stale user(s) out of ~a listed given ~a alive~%"
            (length users) (length acl-users) (length alive-users))
    (loop for s in suffixes
          for uid in to-ungrab
          do
          (progn 
            (loop for d in 
                  (mapcar 'namestring
                          (reduce 
                            'append 
                            (mapcar 'directory devices)))
                  for command :=
                  `("setfacl" "-x"
                        ,(format nil "u:~a" uid)
                        ,d)
                  do
                  (ignore-errors
                    (uiop:run-program
                      command)))
            (sudo::ungrab-devices devices s))
          collect s)))

(defun ungrab-for-stale-chunked (devices n)
  (loop for prev := nil then new
        for new := (ignore-errors (ungrab-for-stale devices n))
        while (not (equal prev new))))

(defun kill-background-process-leaks ()
  (! pkill "Xorg")
  (! pkill -f "/user-lisp-evaluator/socket")
  (restart-lisp-shell-server))



(defun-export sudo::local-resolv-conf ()
              (ask-with-auth
                (:presence t)
                `(local-resolv-conf)))

(defun-export sudo::kill-wifi (&optional (interface "wlan0"))
              (ask-with-auth
                (:presence t)
                `(kill-wifi ,interface)))

(defun-export sudo::nix-collect-garbage ()
              (ask-with-auth (:presence t)
                             `(nix-collect-garbage)))

(defun-export sudo::fuser (file)
  (first
    (second
      (ask-with-auth
        ()
        `(fuser ,(namestring (truename file)))))))

(defun-export sudo::reclaim-file (file &key recursive)
              (ask-with-auth () `(chown-subuser ,file "" ,(or recursive ""))))

(defun-export sudo::rescan-lvm ()
              (ask-with-auth (:presence t) `(rescan-lvm)))

(defun firefox-profile-alive (path)
  (ignore-errors
    (sudo::fuser
      (format nil "~a/cert9.db" (namestring path)))))

(defun firefox-profile-p (path)
  (ignore-errors
    (and (loop for x in '("cert9.db" "key4.db" "search.json")
               unless (probe-file (format nil "~a/~a" (namestring path) x))
               return nil
               finally (return t)))))

(defun cleanup-firefox-profile (path &key force)
  (when (or force (firefox-profile-p path))
    (unless (firefox-profile-alive path)
      (ignore-errors (sudo::reclaim-file path :recursive t))
      (uiop:run-program (list "chmod" "u+rwX" "-R" (namestring path)))
      (uiop:run-program (list "rm" "-r" "-f" (namestring path)))
      path)))

(defvar *shell-init-hooks* nil)
(defun lisp-shell-init () (mapcar 'funcall *shell-init-hooks*))
(push (lambda () (update-firefox-variants :fast t)) *shell-init-hooks*)
(push (lambda () (uiop:setup-temporary-directory)) *shell-init-hooks*)
