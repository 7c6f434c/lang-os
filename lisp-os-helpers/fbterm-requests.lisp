(defpackage :lisp-os-helpers/fbterm-requests
  (:use :common-lisp :lisp-os-helpers/vt :lisp-os-helpers/file-locks :lisp-os-helpers/shell)
  (:export
    #:*fb-device*
    #:*fbterm-settings*
    #:fbterm-request
    ))

(in-package :lisp-os-helpers/fbterm-requests)

(defvar *fb-device* "/dev/fb0")

(defvar *fbterm-settings*
  `((:font-name ,"DejaVu Sans Mono")
    (:font-size ,33)))

(defun run-fbterm (vtdev command
                    &key
                    (fb-device *fb-device*)
                    (fbterm-settings *fbterm-settings*)
                    ignore-error-status
                    )
  (uiop:run-program
    `("env" ,(format nil "FRAMEBUFFER=~a" fb-device)
      "fbterm"
      ,@(loop for s in fbterm-settings 
              collect (format nil "--~a" (string-downcase (first s)))
              collect (format nil "~a" (second s)))
      "--"
      ,@ command)
    :input vtdev :output vtdev :ignore-error-status ignore-error-status))

(defun read-fbterm-value
  (vtdev prompt &key
         (fb-device *fb-device*)
         (fbterm-settings *fbterm-settings*)
         (timeout 15)
         (hide-entry nil)
         )
  (uiop:with-temporary-file
    (:pathname failure-path)
    (uiop:with-temporary-file
      (:pathname value-path)
      (uiop:run-program
        (list "chmod" "u=rw,og="
              (namestring value-path) (namestring failure-path)))
      (uiop:run-program
        (list "chmod" "u=rw,og="
              (namestring value-path) (namestring value-path)))
      (let*
        (
         (script
           (format
             nil
             "
             sayintr() { echo interrupt > ~a ; exit 1; } ; trap sayintr 2;
             echo ~a;
             IFS= read -r -t ~a ~a x || echo fail > ~a;
             echo \"$x\" >~a;
	     true
             "
             (escape-for-shell (namestring failure-path))
             (escape-for-shell prompt)
             timeout
             (if hide-entry "-s" "")
             (escape-for-shell (namestring failure-path))
             (escape-for-shell (namestring value-path))
             ))
         (run-success
	   (run-program-return-success
	     (run-fbterm
	       vtdev (list "sh" "-c" script) 
	       :ignore-error-status t
	       :fbterm-settings fbterm-settings
	       :fb-device fb-device
	       )))
         (complaint
           (string-right-trim '(#\Newline #\Return) 
                              (alexandria:read-file-into-string failure-path)))
         (value
           (string-right-trim '(#\Newline #\Return) 
                              (alexandria:read-file-into-string value-path)))
         (interrupted (equal complaint "interrupt"))
         (timeout (equal complaint "fail"))
         )
        (values
          (unless (or (not run-success) interrupted timeout) value)
          (cond
            ((not run-success) :fbterm-failed)
            (timeout :timeout)
            (interrupted :interrupt)
            (t nil)))))))

(defun fbterm-request
  (prompt &key (vtn 63)
          (file-lock-helper *file-lock-helper*)
          (vt-lock-helper *vt-lock-helper*)
          (vt-lock-directory *vt-lock-directory*)
          (fb-device *fb-device*)
          (hide-entry nil)
          (fbterm-settings *fbterm-settings*)
          (timeout 15)
          (pre-prompt nil)
          (pre-timeout timeout)
          )
  (at-locked-vt
    (vtn
      :vt-lock-helper vt-lock-helper
      :vt-lock-directory vt-lock-directory
      :file-lock-helper file-lock-helper)
    (kill-vt-users vtn)
    (let*
      ((vtdev (format nil "/dev/tty~d" vtn)))
      (unless
        (and
          pre-prompt
          (multiple-value-bind
            (value reason)
            (read-fbterm-value
              vtdev
              (format
                nil "~a~%~a" pre-prompt
                "Press Ctrl-c or Alt-SysRq-k to kill this message and continue; Enter to cancel.")
              :fb-device fb-device
              :fbterm-settings fbterm-settings
              :timeout pre-timeout)
            (or value (eq reason :timeout))))
        (read-fbterm-value
          vtdev prompt
          :fb-device fb-device
          :fbterm-settings fbterm-settings
          :hide-entry hide-entry
          :timeout timeout)))))
