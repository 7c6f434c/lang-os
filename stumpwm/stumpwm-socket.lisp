(defun listen-on-socket (socket-directory)
  (let*
    ((socket-container (format nil "~a/stumpwm-socket" socket-directory))
     (socket-name (format nil "~a/socket" socket-container))
     (socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (ensure-directories-exist socket-name)
    (sb-ext:run-program "rm" (list "-f" socket-name)
                        :search t :wait t)
    (sb-ext:run-program "chmod" (list "u=rwx" "og=" socket-container)
                        :search t :wait t)
    (format *trace-output* "Socket FS preparations done~%")
    (sleep 0.1)
    (sb-bsd-sockets:socket-bind socket socket-name)
    (sb-bsd-sockets:socket-listen socket 3)
    (format *trace-output* "Socket created~%")
    (sb-thread:make-thread
      (lambda ()
        (loop
          for connection := (sb-bsd-sockets:socket-accept socket)
          for stream := (sb-bsd-sockets:socket-make-stream
                          connection
                          :buffering :none
                          :input t :output t
                          :external-format :utf8)
          do (format *trace-output* "Accepted a socket connection~%")
          do (sb-thread:make-thread
               (let
                 ((stream stream))
                 (lambda ()
                   (unwind-protect
                     (let 
                       ((*package* (find-package :stumpwm)))
                       (loop
                         for form := (ignore-errors (read stream nil nil))
                         for dummy := 
                         (format *error-output* "Got form to evaluate:~%~s~%"
                                 form)
                         while (and form (not (eq form :exit)))
                         for result := (multiple-value-bind (result error)
                                         (ignore-errors
                                           (list (eval form)))
                                         (if result 
                                           (first result)
                                           (format nil "Error:~%~a" error)))
                         do (format *error-output* "Evaluated form to:~%~S~%"
                                    result)
                         do (ignore-errors (format stream "~s~%" result)
                                           (finish-output stream))))
                     (ignore-errors (close stream)))))
                 :name "StumpWM socket connection handler")))
      :name "StumpWM socket evaluator")))

