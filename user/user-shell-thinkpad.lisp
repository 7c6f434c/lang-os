; vim:filetype=lisp

(defvar *rc-path*
 (or
  (uiop:getenv "USER_LISP_SHELL_RC")
  *compile-file-pathname*
  *load-pathname*))

(defvar *common-rc*
  (format nil
          "~a/src/nix/lang-os/user/user-shell.lisp"
          (uiop:getenv "HOME")))

(load *common-rc*)
(defun edrc-common () (ed *common-rc*))

(defun im-browsers ()
  (firefox (list "http://chat-judge.in.tum.de"
                 "https://judge.in.tum.de/conpra/jury/clarifications.php"
                 "https://judge.in.tum.de/discuss/c/conpra/"
                 "https://judge.in.tum.de/conpra/jury/submissions.php"
                 "https://judge.in.tum.de/conpra/jury/scoreboard.php"
                 "https://git-judge.in.tum.de/conpra/conpra/wikis/themenliste"
                 "https://git-judge.in.tum.de/problemsetters/problems/merge_requests"
                 "https://www7.in.tum.de/um/courses/praktika/conpra/WS19/"
                 )
           :pass-stderr nil :pass-stdout nil :wait nil
           :no-close t :javascript t :socks-proxy 1080
           :stumpwm-tags "cat/e-im|chat|im|tum|mattermost|conpra|no-auto-tags"))

(defun mccme-webmail-firefox ()
  (firefox (list "https://email.mccme.ru/")
           :pass-stderr nil :pass-stdout nil :wait nil
           :no-close t :stumpwm-tags "cat/em-email email mail mccme no-auto-tags"
           :data "/home/raskin/fallout/"
           :javascript t
           :home t
           :socks-proxy 1080))

(defun github-notifications-firefox (&key (nixos-discourse t))
  (firefox `("https://github.com/notifications?query=is%3Aunread+reason%3Aparticipating"
             ,@(when nixos-discourse `("https://discourse.nixos.org/")))
           :pass-stderr nil :pass-stdout nil :wait nil
           :no-close t :stumpwm-tags "cat/em-email email mail github no-auto-tags"
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
          :stumpwm-tags "cat/e-im im telegram no-auto-tags"
          :javascript t
          :socks-proxy 1080
          :name "telegram-sandbox"
          :home home
          :tmp t
          :profile-storage (format nil "~a/firefox-profile" home)
          :grant (list home))))))

(defun subuser-riot-firefox ()
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
      :stumpwm-tags "cat/e-im im riot matrix no-auto-tags"
      :javascript t
      :socks-proxy 1080
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
      :stumpwm-tags "cat/e-im im riot matrix no-auto-tags nix"
      :javascript t
      :socks-proxy 1080
      :name "nix-riot-sandbox"
      :home home
      :profile-storage (format nil "~a/firefox-profile" home)
      :grant (list home))))

(defun im-windows ()
  (im-browsers)
  (subuser-telegram-firefox)
  (subuser-riot-firefox)
  (&& (subuser-signal))
  )

(defun communication-windows ()
  (ensure-vps-socks)
  ;(matrix-term)
  (im-windows)
  (email-browsers))

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

(defun enter-labri (&rest args &key (brightness 400) (extra-ips `())
                          (location "LaBRI.fr")
                          &allow-other-keys)
  (apply
    'enter-location
    :brightness brightness
    :extra-ips extra-ips
    :location location
    args)
  (! x-options))

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
  (&& (sleep 1) (! web-stream-updater-starter detach))
  (! x-options))

(defun enter-home-poing (&rest args)
  (apply 'enter-home
         (append args
                 (list :location "home@Poing"
                       :extra-requests `((local-resolv-conf)))))
  (ensure-vps-socks)
  (&& (sleep 1) (! web-stream-updater-starter detach)))

(defun enter-home-moscow (&rest args)
  (apply 'enter-home
         (append args
                 (list :location "home@Moscow"
                       :extra-requests `((local-resolv-conf)))))
  (ensure-vps-socks))

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
