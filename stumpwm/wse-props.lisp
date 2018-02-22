(in-package :stumpwm)

(defun raw-window-prop (w name)
  (xlib:get-property
    (window-xwin w)
    (intern (cl-ppcre:regex-replace-all
              "-" (string-upcase name) "_") :keyword)))

(defun window-prop-int (w name)
  (first (raw-window-prop w name)))

(defun window-prop-string (w name)
  (map 'string 'code-char (raw-window-prop w name)))

(defun window-prop-strings (w name)
  (cl-ppcre:split #\Null (window-prop-string w name)))

(defun window-prop-int-p (w name value)
  (equal (window-prop-int w name) value))

(defun window-prop-string-p (w name value)
  (equal (window-prop-string w name) value))

(defun window-hostname (w)
  (window-prop-string w :wm-client-machine))

(defun window-pid (w)
  (window-prop-int w :-net-wm-pid))

(defun window-hostname-p (w value)
  (equal (window-hostname w) value))

(defun window-pid-p (w value)
  (equal (window-pid w) value))

(defun window-locale (w)
  (window-prop-string w :wm-locale-name))

(defun window-command (w)
  (window-prop-strings w :wm-command))
