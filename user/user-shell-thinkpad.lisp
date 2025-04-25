; vim:filetype=lisp

(defvar *rc-path*
 (or
  (uiop:getenv "USER_LISP_SHELL_RC")
  *compile-file-pathname*
  *load-pathname*))

(defparameter *common-rc*
  (format nil
          "~a/user-shell.lisp"
          (cl-ppcre:regex-replace 
            "/[^/]*$" 
            (namestring
              (or 
                *compile-file-pathname*
                *load-pathname*))
            "")))

(load *common-rc*)
(defun edrc-common () (ed *common-rc*))

(defun mccme-webmail-firefox ()
  (firefox (list "https://email.mccme.ru/")
           :pass-stderr nil :pass-stdout nil :wait nil
           :no-close t :stumpwm-tags "cat/em-email email mail mccme no-auto-tags mccme-webmail"
           :data "/home/raskin/fallout/"
           :javascript t
           :home t
           :socks-proxy 1080))

(defun github-notifications-firefox (&key (nixos-discourse t))
  (firefox `("https://github.com/notifications?query=is%3Aunread+reason%3Aparticipating"
             ,@(when nixos-discourse `("https://discourse.nixos.org/")))
           :pass-stderr nil :pass-stdout nil :wait nil
           :no-close t :stumpwm-tags "cat/em-email email mail github no-auto-tags github-web"
           :javascript t
           :socks-proxy 1080))

(defun email-browsers ()
  (mccme-webmail-firefox)
  (github-notifications-firefox))

(defun matrix-term ()
  (& urxvt 
     -name "weechat:matrix:dev" -title "Weechat: Matrix: dev.mccme.ru"
     -e sh -c "https_proxy= weechat -d ~/.weechat-dev"))

(defun vps-term ()
  (& sh -c "ssh-window \"$(cat ~/.vps-ssh)\""))

(defun ensure-vps-socks ()
  (unless (local-port-open-p 1080)
    (vps-term)))

(defun subuser-signal (&key wait verbose)
  (let* ((home (format nil "~a/.local/share/signal-home"
                       (uiop:getenv "HOME"))))
    (loop with queue := (list home)
          while queue
          do (ignore-errors
               (ask-with-auth 
                 ()
                 `(progn
                    ,@(loop for entry in queue collect
                            `(chown-subuser ,entry "")))))
          do (setf queue
                   (loop for entry in queue append
                         (mapcar 'namestring
                                 (directory
                                   (format nil "~a/*.*" entry))))))
    (when verbose (format *trace-output* "Directory grab done~%"))
    (subuser-nsjail-x-application
      (list (true-executable "signal-desktop"))
      :environment `(("HOME" "/signal-home")
                     ("HTTPS_PROXY" "socks://127.0.0.1:1080")
                     ("HTTP_PROXY" "socks://127.0.0.1:1080")
                     ("https_proxy" "socks://127.0.0.1:1080")
                     ("http_proxy" "socks://127.0.0.1:1080")
                     )
      :name "signal-sandbox"
      :netns nil
      :mounts `(("-B" ,home "/signal-home"))
      :grant (list home)
      :hostname "signal-nsjail"
      :wait wait
      :pass-stdout verbose
      :pass-stderr verbose)))

(defun subuser-telegram-firefox (&rest overrides)
  (let* ((home (format nil "~a/.local/share/telegram-home"
                       (uiop:getenv "HOME"))))
    (ignore-errors
      (ask-with-auth 
        ()
        `(chown-subuser ,home "" t)))
    (apply
      'firefox
      (list "https://web.telegram.org/")
      (append
        overrides
        (list
          :pass-stderr nil
          :pass-stdout nil
          :wait nil
          :no-close t 
          :stumpwm-tags "cat/e-im im telegram no-auto-tags telegram-web"
          :javascript t
          :socks-proxy 1080
          :name "telegram-sandbox"
          :home home
          :tmp t
          :slay nil
          :profile-storage (format nil "~a/firefox-profile" home)
          :marionette-socket (format nil "~a/marionette-socket/socket" home)
          :grant (list home))))))

(defun subuser-riot-firefox (&key (socks-proxy 1080))
  (let* ((home (format nil "~a/.local/share/riot-home"
                       (uiop:getenv "HOME"))))
    (ignore-errors
      (ask-with-auth 
        ()
        `(chown-subuser ,home "" t)))
    (firefox
      (list "https://riot.dev.mccme.ru/riot-im/")
      :pass-stderr nil
      :pass-stdout nil
      :wait nil
      :no-close t 
      :stumpwm-tags "cat/e-im im riot matrix no-auto-tags dev.mccme.ru mccme-riot"
      :javascript t
      :socks-proxy socks-proxy
      :name "riot-sandbox"
      :home home
      :profile-storage (format nil "~a/firefox-profile" home)
      :grant (list home))))

(defun subuser-nix-riot-firefox ()
  (let* ((home (format nil "~a/.local/share/riot-nix-home"
                       (uiop:getenv "HOME"))))
    (ignore-errors
      (ask-with-auth 
        ()
        `(chown-subuser ,home "" t)))
    (firefox
      (list "https://app.nitro.chat/")
      :pass-stderr nil
      :pass-stdout nil
      :wait nil
      :no-close t 
      :stumpwm-tags "cat/e-im im riot matrix no-auto-tags nix nix-riot"
      :javascript t
      :socks-proxy 1080
      :name "nix-riot-sandbox"
      :home home
      :profile-storage (format nil "~a/firefox-profile" home)
      :grant (list home))))

(defun subuser-tum-riot-firefox ()
  (let* ((home (format nil "~a/.local/share/riot-tum-home"
                       (uiop:getenv "HOME"))))
    (ignore-errors
      (ask-with-auth 
        ()
        `(chown-subuser ,home "" t)))
    (firefox
      (list "https://matrix.in.tum.de/")
      :pass-stderr nil
      :pass-stdout nil
      :wait nil
      :no-close t 
      :stumpwm-tags "cat/e-im im riot matrix no-auto-tags in.tum.de"
      :javascript t
      :socks-proxy 1080
      :name "tum-riot-sandbox"
      :home home
      :profile-storage (format nil "~a/firefox-profile" home)
      :grant (list home))))

(defun subuser-midpass-firefox (&rest overrides)
  (let* ((home (format nil "~a/.local/share/midpass-home"
                       (uiop:getenv "HOME"))))
    (ignore-errors
      (ask-with-auth 
        ()
        `(chown-subuser ,home "" t)))
    (apply
      'firefox
      (list )
      (append
        overrides
        (list
          :late-urls (list "https://midpass.ru/")
          :extensions (mapcar 'namestring (directory (~ ".nix-personal/personal-firefox-extensions-result/*.*")))
          :pass-stderr nil
          :pass-stdout nil
          :wait nil
          :no-close t 
          :stumpwm-tags "midpass"
          :javascript t
          :socks-proxy 1080
          :name "midpass-sandbox"
          :home home
          :tmp t
          :profile-storage (format nil "~a/firefox-profile" home)
          :grant (list home))))))

(defun enter-ratmino (&rest args &key (brightness 20) (extra-ips `())
                          (location "ratmino")
                          &allow-other-keys)
  (apply
    'enter-location
    :brightness brightness
    :extra-ips extra-ips
    :location location
    args)
  (! x-options))

(defun enter-mccme (&rest args &key (brightness 20) (extra-ips `())
                          (location "mccme")
                          &allow-other-keys)
  (apply
    'enter-location
    :brightness brightness
    :extra-ips extra-ips
    :location location
    args)
  (! x-options))

(defun enter-tum (&rest args &key (brightness 400) (extra-ips `())
                        (location "in.tum.de")
                        &allow-other-keys)
  (! x-options)
  (apply
    'enter-location
    :brightness brightness
    :extra-ips extra-ips
    :location location
    :skip-wifi
    (or
      (ignore-errors (ethernet-attached "eth0"))
      (ignore-errors (ethernet-attached "eth1")))
    :extra-requests `((activate-interface "eth0")
                      (activate-interface "eth1")
                      (dhcp-resolv-conf))
    args)
  (! x-options)
  (when
    (or
      (ignore-errors (ethernet-attached "eth0"))
      (ignore-errors (ethernet-attached "eth1"))
      )
    (ask-with-auth
      (:presence t)
      `(progn
         ,(if (ethernet-attached "eth0") `(dhclient "eth0" t) `(progn))
         ,(if (ethernet-attached "eth1") `(dhclient "eth1" t) `(progn))
         ))
    (! proxy-restart (format nil "~a/src/rc/squid/direct.squid" ($ :home))))
  (ensure-vps-socks)
  (! x-options))

(defun enter-home-poing (&rest args)
  (apply 'enter-home
         (append args
                 (list :location "home@Poing"
                       :extra-requests `((local-resolv-conf)))))
  (ensure-vps-socks))

(defun enter-home-moscow (&rest args)
  (apply 'enter-home
         (append args
                 (list :location "home@Moscow"
                       :extra-requests `((local-resolv-conf)))))
  (ensure-vps-socks))

(defun enter-home-bordeaux-yser (&rest args)
  (apply 'enter-location
         (append args
                 (list :location "home@Bordeaux"
                       :brightness 400
                       :skip-wifi t
                       :extra-requests `(
                                         (set-brightness 45)
                                         (set-brightness 400)
                                         (empty-resolv-conf)
                                         (sleep 1)
                                         (dhclient "eth1" t nil nil nil 
                                                   ,(first (uiop:run-program 
                                                             (list "hostname") 
                                                             :output :lines))
                                                   t)))))
  (! xrandr --fb 4000x3000)
  (sleep 0.1)
  (! xrandr --output "VGA-1-2" --mode 1920x1080 --left-of "LVDS-1")
  (sleep 0.1)
  (! xrandr --fb 4000x3000)
  )

(defun enter-labri (&rest args)
  (apply 'enter-location
         (append args
                 (list :location "LaBRI.fr"
                       :brightness 400
                       :skip-wifi t
                       :extra-requests `((list
                                           (set-brightness 45)
                                           (set-brightness 400)
                                           (dhclient "eth1" t)
                                           (dhcp-resolv-conf))))))
  (format *trace-output* "enter-labri has called enter-location~%")
  (unless
    (let ((address 
            (getf
              (find
                "inet"
                (getf
                  (first
                    (lisp-os-helpers/network::parsed-ip-address-show
                      "eth1"))
                  :addresses)
                :test 'equalp 
                :key (lambda (x) (getf x :address-type)))
              :address)))
      (format *trace-output* "ip address on eth1: ~s~%" address)
      (and address (not (cl-ppcre:scan "^169[.]254[.]" address))))
    (format *trace-output* "enter-labri calling dhclient on eth1~%")
    (ask-with-auth
      (:presence t)
      `(list
         (usb-eth-reload-modules)
         (sleep 1.5)
         (dhclient "eth1" t)
         )))
  (format *trace-output* "enter-labri network done~%")
  (! xrandr --fb 4000x3000)
  (sleep 0.1)
  (! xrandr --output "VGA-1-2" --mode 1920x1080 --right-of "LVDS-1")
  (sleep 0.1)
  (! xrandr --fb 4000x3000)
  (format *trace-output* "enter-labri screen done~%")
  (! keymap-more-symbols)
  (format *trace-output* "enter-labri keyboard done~%")
  (format *trace-output* "enter-labri done~%")
  )

(defun disconnect (&key kill-ssh kill-wifi kill-bg (brightness 1) (cpu-frequency "min")
                        kill-matrixcli standby-options standby kill-mounts)
  (when kill-mounts
    (loop for d in (directory (~ "mnt/*/")) do
          (& fusermount -u (namestring d))))
  (alexandria:write-string-into-file
    "10" (format nil "~a/.watchperiod" (uiop:getenv "HOME"))
    :if-exists :supersede)
  (! web-stream-updater-starter quit)
  (uiop:run-program "rm ~/.update-web-streams-*" :ignore-error-status t)
  (ask-with-auth 
    (:presence t)
    `(list 
       (set-cpu-frequency ,cpu-frequency)
       (set-brightness ,brightness)
       ,@(when kill-wifi `((kill-wifi "wlan0")))))
  (when kill-ssh
    (ignore-errors (stumpwm-eval `(close-ssh-windows)))
    (! pkill "-HUP" ssh-fwd)
    (! pkill "-HUP" -f /ssh-fwd))
  (when kill-bg (kill-background-process-leaks))
  (when kill-matrixcli
    (! pkill -f /matrixcli)
    (! pkill -f " matrixcli"))
  (! x-options)
  (when standby (apply 'standby standby-options))
  )

(defun xrandr-home-poing ()
  (! xrandr --output "VGA-1-2" --above "LVDS-1")
  (! xrandr --fb 4000x3000)
  (! pkill compton)
  (sleep 0.1)
  (! pkill -abrt compton)
  (sleep 0.1)
  (! pkill -kill compton)
  (sleep 0.5)
  (& compton --dbe))

(defun unlock-phone ()
  (sudo:tether-android)
  (sudo:tether-android)
  (! unlock-phone))

(defun webrtc-chromium (url &rest args)
  (apply 'subuser-nsjail-x-application (list (truename "/home/raskin/.nix-personal/hydra-grab/chromium/bin/chromium") "--no-sandbox" url) :grab-sound t :netns nil :pass-stderr t :grab-dri t :grab-camera t :home t :tmp t  :full-dev t :fake-groups `("audio" "video") :with-dbus t :fake-passwd t args))

(defun comm-browsers (&key (intra-sleep 5) (post-sleep 60))
  (loop for arglist in
        `(
          ("my-vps-ssh" vps-term)
          ("telegram-web" subuser-telegram-firefox)
          ("mccme-riot" subuser-riot-firefox)
          ("nix-riot" subuser-nix-riot-firefox)
          ("github-web" github-notifications-firefox)
          ("ub-webmail" firefox (
                                 "https://webmel.u-bordeaux.fr/"
                                 "https://celcat.u-bordeaux.fr/"
                                 "https://moodle.u-bordeaux.fr/"
                                 "https://gitlab.emi.u-bordeaux.fr"
                                 "https://cas.u-bordeaux.fr/"
                                 )
           :prefs (("browser.urlbar.suggest.bookmark" t))
           :bookmarks (
                       "https://ent.u-bordeaux.fr"
                       ("https://diff.u-bordeaux.fr/sympa/" "Mailing list management for UBx")
                       ("https://gds.labri.fr/index.php" "Room reservations at LaBRI")
                       "https://www.labri.fr/intranet"
                       "https://hyperplanning.iut.u-bordeaux.fr/"
                       "https://gitlab.emi.u-bordeaux.fr/pt2/teams/"
                       "https://sgse.u-bordeaux.fr/sgseub/"
                       "https://algodist.labri.fr/index.php/Main/GT"
                       ("https://applis.u-bordeaux.fr/ent/annuaire/diffusion.php" 
                        "Mailing lists e.g. student groups")
                       "https://www.u-bordeaux.fr/annuaire"
                       "https://www.labri.fr/presentation/annuaire"
                       "https://chat.u-bordeaux.fr/"
                       ("https://services.emi.u-bordeaux.fr/edt/" "CREMI room use schedule")
                       ("https://apogee.u-bordeaux.fr/snw/" "Grade submission online")
                       "https://evento.renater.fr/"
                       ("https://apogee.u-bordeaux.fr/trombi/" "Student photos")
                       "https://apps-ent.u-bordeaux.fr/annuaire/v2/index.php"
                       "https://nuxeo.u-bordeaux.fr/"
                       "https://celcat.u-bordeaux.fr/"
                       "https://moodle.u-bordeaux.fr/"
                       "https://gitlab.emi.u-bordeaux.fr"
                       "https://webmel.u-bordeaux.fr/"
                       "https://gitub.u-bordeaux.fr/"
                       )
           :javascript t :tmp t :hostname-suffix "ub." :home t
           :no-close t 
           :wait nil
           :pass-stderr nil :pass-stdout nil
           :stumpwm-tags 
           "cat/em-email|email|webmail|u-bordeaux|chat|im|webchat|ub-chat|ub-mail|ub-email|ub-msg|ub-messaging|no-auto-tags|ub-webmail")
          ("mccme-webmail" mccme-webmail-firefox)
          ("acx-discord" firefox 
           ("https://discordapp.com/") 
           :javascript t :marionette-socket nil :tmp t :home t 
           :hostname-suffix "acx-discord." 
           :wait nil
           :pass-stderr nil :pass-stdout nil
           :stumpwm-tags 
           "no-auto-tags|discord|acx-discord|im|webchat|cat/e-im")
          ("nixpkgs-zulip" firefox
           ("https://nixpkgs.zulipchat.com/") 
           :javascript t 
           :stumpwm-tags "nixpkgs-zulip im webchat cat/e-im" 
           :no-close t
           :pass-stderr nil :pass-stdout nil
           :wait nil)
          ("ub-discord" firefox 
           ("https://discordapp.com/") 
           :javascript t :marionette-socket nil :tmp t :home t 
           :hostname-suffix "ub-discord." 
           :wait nil
           :pass-stderr nil :pass-stdout nil
           :stumpwm-tags 
           "no-auto-tags|ub|u-bordeaux|discord|ub-discord|im|webchat|cat/e-im")
          )
        do
        (let ((arglist arglist))
          (unless
            (stumpwm-eval
              `(act-on-matching-windows
                 (w :screen)
                 (tagged-p w ,(string-upcase (first arglist)))
                 (list (window-id w) (window-title w))))
            (ignore-errors
              (apply 'funcall (rest arglist))
              t)
            (sleep intra-sleep))))
  (sleep post-sleep))

(defun comm-browsers-number-tags ()
  (loop for role in 
        `(
          ("my-vps-ssh" 8)
          ("telegram-web" 0)
          ("mccme-riot" 1)
          ("nix-riot" 2)
          ("github-web" 3)
          ("ub-webmail" 4)
          ("mccme-webmail" 5)
          ("nixpkgs-zulip"7)
          ("ub-discord" 9)
          ("acx-discord" 6)
          )
        do
        (stumpwm-eval
          `(act-on-matching-windows
             (w :screen)
             (tagged-p w ,(string-upcase (first role)))
             (tag-window ,(format nil "~a" (second role)) w)))))
