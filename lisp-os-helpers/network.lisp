(defpackage :lisp-os-helpers/network
  (:use :common-lisp :lisp-os-helpers/shell 
        :lisp-os-helpers/daemon)
  (:export
    #:parsed-ip-address-show
    #:add-ip-address
    #:remove-ip-address
    #:flush-ip-addresses
    #:enable-ip-link
    #:disable-ip-link
    #:run-link-dhclient
    #:stop-link-dhclient
    #:port-open-p
    #:wpa-supplicant-status
    #:ensure-wpa-supplicant
    #:restart-wpa-supplicant
    #:start-wpa-supplicant
    #:stop-wpa-supplicant
    #:wpa-supplicant-wait-connection
    #:wpa-supplicant-running-p
    #:get-default-gateway
    #:local-resolv-conf
    #:dhcp-resolv-conf
    #:router-resolv-conf
    #:local-port-open-p
    #:parsed-wpa-network-list
    #:wpa-set-network-status-by-id
    #:wpa-set-network-status-by-essid
    #:wpa-set-enabled-network-id-list
    #:wpa-set-enabled-network-essid-list
    ))
(in-package :lisp-os-helpers/network)

(defun attribute-labels-to-keywords (l)
  (loop
    for p := l then (cddr p)
    for k := (first p)
    for v := (second p)
    for ks := (intern (string-upcase k) :keyword)
    while v
    collect ks collect v))

(defun parse-ip-address-line (line)
  (cond
    ((equal (subseq line 0 7) (make-string 7 :initial-element #\Space))
     (attribute-labels-to-keywords
       (cl-ppcre:split " " (string-trim " " line))))
    ((equal (subseq line 0 4) (make-string 4 :initial-element #\Space))
     (let*
       ((components (cl-ppcre:split " " (string-trim " " line)))
        (address-type (first components))
        (address-line (second components))
        (address-components (cl-ppcre:split "/" address-line))
        (address (first address-components))
        (mask-length (second address-components)))
       (append
         (list
           :address-type address-type
           :address address
           :netmask-length mask-length)
         (attribute-labels-to-keywords (cddr components)))))
    (t
      (let*
        ((components (cl-ppcre:split " " (string-trim " " line)))
         (interface-index (parse-integer (string-trim ":" (first components))))
         (interface-name (string-trim ":" (second components)))
         (interface-attributes (third components))
         (interface-attribute-components
           (remove "" (cl-ppcre:split "[<>,]" interface-attributes)
                   :test 'equal)))
        (append
          (list
            :interface-index interface-index
            :interface-name interface-name
            )
          (loop
            for a in interface-attribute-components
            collect (intern (string-upcase
                              (cl-ppcre:regex-replace-all "_" a "-"))
                            :keyword)
            collect t)
          (attribute-labels-to-keywords
            (cdddr components)))))))

(defun parsed-ip-address-show (&optional interface)
  (let*
    ((lines 
       (program-output-lines
         `("ip" "address" "show" ,@(when interface `(,interface)))))
     (parsed-lines
       (mapcar 'parse-ip-address-line lines))
     (interfaces
       (loop
         with interface := nil
         with addresses := nil
         with address := nil
         for l in (append parsed-lines (list nil))
         when (or (null l) (getf l :interface-name) (getf l :address))
         do (progn
              (when address (push address addresses))
              (setf address nil))
         when (or (null l) (getf l :interface-name))
         do (progn
              (setf 
                interface
                (append 
                  interface 
                  (list 
                    :addresses (reverse addresses) 
                    :addresses-global-brief
                    (loop
                      for a in addresses
                      when (equal (getf a :scope) "global")
                      collect (getf a :address)))))
              (setf addresses nil))
         if (and (or (null l) (getf l :interface-name))
                 (getf interface :interface-name))
         collect interface
         else if (or (null l) (getf l :interface-name))
         do (progn)
         else if (getf l :address)
         do (setf address l)
         else
         do (setf address (append address l))
         when (getf l :interface-name) do (setf interface l)
         when (getf l :address) do (setf address l)
         )))
    interfaces))

(defun ip-address-info (interface address)
  (find-if
    (lambda (x) (equal (getf x :address) address))
    (getf (first (parsed-ip-address-show interface)) :addresses)))

(defun add-ip-address (interface address &optional (netmask-length 24))
  (uiop:run-program
    (list "ip" "address"
          "add" (format nil "~a/~a" address netmask-length)
          "dev" interface)))

(defun remove-ip-address (interface address)
  (uiop:run-program
    (list "ip" "address"
          "delete"
          (format
            nil "~a/~a"
            address
            (getf (ip-address-info interface address) :netmask-length))
          "dev" interface)))

(defun flush-ip-addresses (interface &key family)
  (uiop:run-program
    `("ip"
      ,@(when family (list (format nil "-~a" family)))
      "address" "flush" "dev" ,interface)))

(defun enable-ip-link (interface)
  (uiop:run-program
    (list "ip" "link" "set" interface "up")))

(defun disable-ip-link (interface)
  (uiop:run-program
    (list "ip" "link" "set" interface "down")))

(defun run-link-dhclient (interface &key no-resolv once)
  (run-program-return-success
    (uiop:run-program
      (list "dhcpcd" "-x" interface)))
  (run-program-return-success
    (uiop:run-program
      (list "cp" "/etc/resolv.conf" "/etc/resolv.conf.dhclient")))
  (run-program-return-success
    (uiop:run-program
      `("dhcpcd"
        "-p" ,interface 
        ,@(when no-resolv `("-C" "resolv.conf"))
        ,@(when once `("-1"))
        )))
  (run-program-return-success
    (uiop:run-program
      (list "cp" "/etc/resolv.conf" "/etc/resolv.conf.dhclient-new")))
  (run-program-return-success
    (uiop:run-program
      (list "cp" "/etc/resolv.conf.dhclient" "/etc/resolv.conf"))))

(defun stop-link-dhclient (interface)
  (run-program-return-success
    (uiop:run-program
      (list "dhcpcd" "-x" interface))))

(defun port-open-p (port &key (host "127.0.0.1"))
  (ignore-errors
    (iolib/sockets:make-socket
      :remote-host host :remote-port port)
    t))

(defun wpa-supplicant-status (interface)
  (let*
    ((lines
       (program-output-lines
	 `("wpa_cli" "status" "-i" ,interface))))
    (loop
      for l in lines
      for p := (cl-ppcre:split "=" l)
      for k := (first p)
      for v := (second p)
      collect (intern (string-upcase k) :keyword)
      collect v)))

(defun wpa-supplicant-running-p (interface)
  (ignore-errors
    (wpa-supplicant-status interface)
    t))

(defun start-wpa-supplicant (interface config-file &key driver)
  (daemon-with-logging
    "daemon/wpa-supplicant"
    (list "wpa_supplicant" "-i" interface "-c" config-file
          "-D" (or driver "nl80211"))))

(defun stop-wpa-supplicant (interface)
  (uiop:run-program
    (list "wpa_cli" "-i" interface "terminate")
    :ignore-error-status t))

(defun ensure-wpa-supplicant (interface config-file &key driver)
  (unless
    (wpa-supplicant-running-p interface)
    (start-wpa-supplicant interface config-file :driver driver)))

(defun restart-wpa-supplicant (interface config-file &key driver)
  (stop-wpa-supplicant interface)
  (start-wpa-supplicant interface config-file :driver driver))

(defun wpa-supplicant-wait-connection
  (interface &key
             (timeout 30) (sleep 0.2) (state "COMPLETED"))
  (loop
    with start-time := (get-universal-time)
    for current-state := (ignore-errors
                           (getf (wpa-supplicant-status interface)
                                 :wpa_state))
    while (< (- (get-universal-time) start-time) timeout)
    when (equalp current-state state) return t
    do (sleep sleep)))

(defun get-default-gateway ()
  (let* ((lines (program-output-lines
                  `("ip" "route" "show" "default")))
         (first-line (first lines))
         (via (cl-ppcre:regex-replace-all
                " .*"
                (cl-ppcre:regex-replace-all
                  "^.* via " first-line "") "")))
    via))

(defun local-resolv-conf (&optional search)
  (with-open-file
    (f "/var/etc/resolv.conf" :direction :output :if-exists :supersede)
    (when search
      (format f "search ~a~%" search))
    (format f "nameserver 127.0.0.1~%")))

(defun dhcp-resolv-conf ()
  (alexandria:write-string-into-file
    (concatenate
      'string
      (alexandria:read-file-into-string
        "/etc/resolv.conf.dhclient")
      (alexandria:read-file-into-string
        "/etc/resolv.conf.dhclient-new"))
    "/etc/resolv.conf"
    :if-exists :supersede))

(defun router-resolv-conf (&optional search)
  (with-open-file
    (f "/var/etc/resolv.conf" :direction :output :if-exists :supersede)
    (when search
      (format f "search ~a~%" search))
    (format f "nameserver ~a~%" (get-default-gateway))))

(defun local-port-open-p (port &optional (protocol :tcp) (interface "*"))
  (uiop:run-program
    (list "ss" "-l" "-n" "-H"
          (format nil "--~a" (string-downcase protocol))
          "src" (format nil "~a:~a" interface port))
    :output :lines))

(defun parse-wpa-network-list-line (s)
  (let* ((entries (cl-ppcre:split (string #\Tab) s)))
    (list :id (parse-integer (first entries))
          :essid (second entries)
          :bssid (third entries)
          :flags (remove "" (cl-ppcre:split "[ \\[\\]]+" (fourth entries))
                         :test 'equal))))

(defun parsed-wpa-network-list (&optional (interface "wlan0"))
  (let* ((lines (program-output-lines
                  `("wpa_cli" "list_networks" "-i" ,interface)))
         (data-lines (rest lines))
         (parsed-lines (mapcar 'parse-wpa-network-list-line data-lines)))
    parsed-lines))

(defun wpa-set-network-status-by-id (id enablep &optional (interface "wlan0"))
  (uiop:run-program
    `("wpa_cli" ,(if enablep "enable_network" "disable_network")
      "-i" ,interface ,(format nil "~a" id))))

(defun wpa-set-network-status-by-essid (essid enablep &optional (interface "wlan0"))
  (loop for n in (parsed-wpa-network-list interface)
        for curessid := (getf n :essid)
        for curid := (getf n :id)
        when (equal essid curessid)
        do (wpa-set-network-status-by-id curid enablep interface)))

(defun wpa-set-enabled-network-id-list (ids &optional (interface "wlan0") except)
  (loop for n in (parsed-wpa-network-list interface)
        for curid := (getf n :id)
        do (wpa-set-network-status-by-id
             curid (if (find curid ids) (not except) except) interface)))

(defun wpa-set-enabled-network-essid-list (essids &optional (interface "wlan0") except)
  (loop for n in (parsed-wpa-network-list interface)
        for curid := (getf n :id)
        for curessid := (getf n :essid)
        do (wpa-set-network-status-by-id
             curid (if (find curessid essids :test 'equal) (not except) except) interface)))
