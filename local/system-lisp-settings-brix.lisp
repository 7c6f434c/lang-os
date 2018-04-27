(defparameter *auto-wifi* `("wlan0"))
(defparameter *auto-interfaces* '("eth0"))
(defparameter *auto-ip-addresses* '(("wlan0" "192.168.0.202")))
(defparameter *auto-modules* '("rtl8821ae"))

(defparameter *auto-acls* `(("/dev/kvm" "u:ofborg:rwx") ("/dev/kvm" "g:nixbld:rwx")))
