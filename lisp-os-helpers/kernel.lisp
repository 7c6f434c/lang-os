(defpackage :lisp-os-helpers/kernel
  (:use
    :common-lisp
    :lisp-os-helpers/shell
    )
  (:export
    #:modprobe
    #:module-remove
    #:module-remove-recursive
    #:set-brightness
    #:set-cpu-frequency
    #:unmount-removable
    #:power-state
    ))
(in-package :lisp-os-helpers/kernel)

(defun modprobe (module &rest parameters)
  (uiop:run-program
    `("/var/current-system/bin/modprobe"
      ,module
      ,@(loop
          for p in parameters
          collect
          (cond
            ((stringp p) p)
            ((listp p)
             (format nil "~a=~a" (first p) (or (second p) "")))
            (t (format nil "~a" p)))))))

(defun module-remove-recursive (module)
  (loop
    for l in (program-output-lines `("lsmod"))
    for data := (cl-ppcre:split " +[0-9]+ +[0-9]+ *" l)
    when (equal (first data) module)
    do
    (mapcar 'module-remove-recursive
            (cl-ppcre:split "," (second data))))
  (run-program-return-success
    (uiop:run-program
      `("/var/current-system/bin/modprobe" "-r" ,module))))

(defun module-remove (module)
  (uiop:run-program
    `("/var/current-system/bin/modprobe" "-r" ,module)))

(defun set-brightness (n)
  (let*
    ((f
       (loop
         for name in
         (list
           "intel_backlight" "acpi_backlight" "acpi_backlight0"
           "nv_backlight" "radeon_backlight")
         for file := (format nil "/sys/class/backlight/~a/brightness" name)
         when (probe-file file) return file
         finally (return (first (directory "/sys/class/backlight/*/brightness"))))))
    (alexandria:write-string-into-file
      (format nil "~a" n) f :if-exists :overwrite)))

(defun set-cpu-frequency (n)
  (let*
    ((governor
       (cond
         ((find n '(:auto :ondemand)) "ondemand")
         (t "userspace")))
     (cpus
       (mapcar 'namestring
               (directory
                 "/sys/devices/system/cpu/cpu[0123456789]*/cpufreq/"))))
    (loop
      for cpu in cpus
      for frequency :=
      (cond
        ((numberp n) (format nil "~a" (* n 1000)))
        ((find n '(:min nil))
         (first (file-lines (format nil "~a/scaling_min_freq" cpu))))
        ((find n '(:max t))
         (first (file-lines (format nil "~a/scaling_max_freq" cpu)))))
      do (overwrite-file
           governor (format nil "~a/scaling_governor" cpu))
      when frequency
      do (overwrite-file
           frequency (format nil "~a/scaling_setspeed" cpu)))))

(defun unmount-removable ()
  (let*
    ((masks `("/dev/nbd*" "/dev/sd[bcdefg]*" "/dev/mmcblk*" "/home/*/mnt/*"))
     (list (mapcar 'namestring (apply 'append (mapcar 'directory masks)))))
    (loop
      for entry in list do
      (uiop:run-program (list "umount" entry) :ignore-error-status t))))

(defun power-state (state)
  (unmount-removable)
  (case state
    (:mem (overwrite-file "mem" "/sys/power/state"))
    (:disk
      (overwrite-file "shutdown" "/sys/power/disk")
      (overwrite-file "mem" "/sys/power/state"))
    (:both
      (overwrite-file "suspend" "/sys/power/disk")
      (overwrite-file "mem" "/sys/power/state"))))

