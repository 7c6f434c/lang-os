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
 (format
  t "System: ~s~%"
  (nix-build "systemInstance"
   :nix-file path
   :nix-path (if (stringp nix-path)
	   (cl-ppcre:split ":" nix-path)
	   nix-path)))
 (with-system-socket
  ()
  (ask-server
   (with-password-auth
    "Rebuild the system"
    `(rebuild-from-path ,(namestring (truename path)) ,nix-path)
    :user "root"))))

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
                        data
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
    :network-ports
    `(
      ,@(when dns `( ((53 :udp)) ((53 :tcp))  ))
      ,@(when http-proxy `(((3128 tcp))))
      ,@(when socks-proxy `(((1080 tcp))))
      ,@ network-ports)
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
      )
    :prefs
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
    :mounts
    `(
      ,@ mounts
      ,@ (when file `(("-B" ,file)))
      ,@ (when data `(("-B" ,data "/home/data")))
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
        `(ensure-wifi ,interface ,dhclient)))))
