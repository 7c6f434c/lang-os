(defpackage :lisp-os-helpers/file-locks
  (:use :common-lisp)
  (:export
    #:with-file-lock
    #:*file-lock-helper*
    ))

(in-package :lisp-os-helpers/file-locks)

(defvar *file-lock-helper* "/run/current-system/sw/bin/file-lock")

(defmacro with-file-lock ((path &key (file-lock-helper *file-lock-helper*)) &body body)
  `(progn
     (ensure-directories-exist ,path)
     (let*
       ((locker-process (uiop:launch-program (list ,file-lock-helper ,path)
                                             :input :stream :output :stream))
        (locker-input (uiop:process-info-input locker-process))
        (locker-output (uiop:process-info-output locker-process)))
       (unwind-protect
         (progn
           (read-line locker-output)
           ,@body)
         (format locker-input "OK~%")
         (force-output locker-input)
         (close locker-input)
         (close locker-output)
         (uiop:terminate-process locker-process)
         (sleep 0.001)
         (uiop:terminate-process locker-process :urgent t)))))

