(defpackage :lisp-os-helpers/vt
  (:use :common-lisp :lisp-os-helpers/shell :lisp-os-helpers/file-locks)
  (:export
    #:get-fg-vt
    #:run-in-vt
    #:kill-vt-users
    #:chvt
    #:with-vt-switches
    #:vt-lock
    #:*vt-lock-helper*
    #:*vt-lock-directory*
    #:at-locked-vt
    ))

(in-package :lisp-os-helpers/vt)

(defun get-fg-vt ()
  (parse-integer (uiop:run-program "fgconsole" :output :string)))

(defun run-in-vt (command &key vt)
  (uiop:run-program
    `("setsid" "-w"
      "openvt" "-s" "-e" "-v"
      ,@(when vt `("-c" ,(format nil "~a" vt) "-f"))
      "--"
      ,@(if (stringp command)
          `("/bin/sh" "-c" ,command)
          command))))

(defun kill-vt-users (vt)
  (uiop:run-program `("fuser" "-k" "-15" ,(format nil "/dev/tty~a" vt)))
  (sleep 0.3)
  (uiop:run-program `("fuser" "-k" "-9" ,(format nil "/dev/tty~a" vt))))

(defun chvt (n)
  (uiop:run-program (list "chvt" (format nil "~a" n))))

(defmacro with-vt-switches (&body body)
  (let
    ((vt (gensym)))
    `(let
       ((,vt (get-fg-vt)))
       (prog1
         (progn
           ,@body)
         (chvt ,vt)))))

(defvar *vt-lock-helper* "/run/current-system/sw/bin/vtlock")

(defun vt-lock (lockp &key (vt-lock-helper *vt-lock-helper*))
  (uiop:run-program
    (list (or vt-lock-helper *vt-lock-helper*) (if lockp "yes" "no"))))

(defvar *vt-lock-directory* "/run/vt-locks/")

(defmacro at-locked-vt ((vtn &key (vt-lock-helper *vt-lock-helper*)
                             (vt-lock-directory *vt-lock-directory*) 
                             (file-lock-helper *file-lock-helper*))
                        &body body)
  (let*
    ((vtold (gensym)))
    `(let*
       ((,vtold (get-fg-vt)))
       (with-file-lock
         ((format nil "~a/~a" ,vt-lock-directory ,vtn)
          :file-lock-helper ,file-lock-helper)
         (unwind-protect
           (progn
             (chvt ,vtn)
             (vt-lock t :vt-lock-helper ,vt-lock-helper)
             ,@body)
           (vt-lock nil :vt-lock-helper ,vt-lock-helper)
           (chvt ,vtold))))))

