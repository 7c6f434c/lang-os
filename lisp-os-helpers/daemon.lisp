(defpackage :lisp-os-helpers/daemon
  (:use :common-lisp :lisp-os-helpers/shell :lisp-os-helpers/timestamp)
  (:export
    #:file-used
    #:file-used-by
    #:file-used-by-pid
    #:file-still-used
    #:console-used
    #:periodically
    #:daemon-with-logging
    #:system-service
    #:kill-by-log
    #:kill-by-executable
    #:kill-by-files
    #:kill-by-uid
    ))
(in-package :lisp-os-helpers/daemon)

(defun file-used (filename)
  (run-program-return-success
    (uiop:run-program (list "fuser" filename))))

(defun file-used-by (filename)
  (mapcar
    'parse-integer
    (remove 
      ""
      (cl-ppcre:split
        " "
        (uiop:run-program
          (list "fuser" filename)
          :output :string
          :ignore-error-status t))
      :test 'equal)))

(defun file-used-by-pid (filename pid)
  (member
    filename
    (mapcar
      'namestring
      (directory
        (format nil "/proc/~a/fd/*" (or pid "self"))))
    :test 'equal))

(defparameter *file-use-cache* (make-hash-table :test 'equal))

(defun file-still-used (filename)
  (let*
    ((still-used-pids
       (loop for p in (gethash filename *file-use-cache*)
             when (file-used-by-pid filename p)
             collect p))
     (use-pids (or still-used-pids
                   (file-used-by filename))))
    (setf (gethash filename *file-use-cache*) use-pids)
    use-pids))

(defun console-used (n)
  (file-still-used (format nil "/dev/tty~d" n)))

(defmacro periodically ((period &key silently) &body body)
  (let*
    ((res (gensym)))
    `(loop
       for ,res := (list ,@body)
       ,@(unless silently
           `(
             do (format t "Results:~%~{~s~%~}" ,res)
             do (format t "~s~%~%" (local-time:now))))
       do (sleep ,period))))

(defmacro with-logging ((name) &body body)
  (let
    ((timestamp (gensym))
     (stdout-log (gensym))
     (stderr-log (gensym))
     (stdout-log-link (gensym))
     (stderr-log-link (gensym))
     )
    `(progn
       (ensure-directories-exist
	 (format nil "/var/log/system-lisp-logs/~a/" ,name))
       (let*
	 ((,timestamp (timestamp))
	  (,stdout-log 
	    (format
	      nil
	      "/var/log/system-lisp-logs/~a/stdout-~a.log"
	      ,name ,timestamp))
	  (,stderr-log
	    (format
	      nil
	      "/var/log/system-lisp-logs/~a/stderr-~a.log"
	      ,name ,timestamp))
	  (,stdout-log-link
	    (format
	      nil
	      "/var/log/system-lisp-logs/~a/stdout-freshest.log"
	      ,name))
	  (,stderr-log-link
	    (format
	      nil
	      "/var/log/system-lisp-logs/~a/stderr-freshest.log"
	      ,name))
	  )
	 (with-open-file
	   (*standard-output* ,stdout-log :direction :output)
	   (with-open-file
	     (*error-output* ,stderr-log :direction :output)
	     (uiop:run-program (list "ln" "-sfT" ,stdout-log ,stdout-log-link))
	     (uiop:run-program (list "ln" "-sfT" ,stderr-log ,stderr-log-link))
	     ,@body))))))

(defun run-with-logging (name command &rest options)
  (with-logging
    ((format 
       nil "~a/~a"
       name (cl-ppcre:regex-replace-all "^.*/" command "")))
    (apply 
      'uiop:run-program command
      :output *standard-output* :error-output *error-output*
      :input "/dev/null" options)))

(defun start-with-logging (name command &rest options)
  (with-logging
    ((format 
       nil "~a/~a"
       name (cl-ppcre:regex-replace-all "^.*/" command "")))
    (apply 
      'uiop:launch-program command
      :output *standard-output* :error-output *error-output*
      :input "/dev/null" options)))

(defun daemon-with-logging (name command &rest options)
  (with-logging
    ((format 
       nil "~a/~a"
       name (cl-ppcre:regex-replace-all "^.*/" (first command) "")))
    (apply 
      'uiop:run-program (cons "setsid" command)
      :output *standard-output* :error-output *error-output*
      :input "/dev/null" options)))

(defun system-service (log service &rest arguments)
  (daemon-with-logging 
    (format nil "daemon/~a" log)
    (append
      (list
	(format nil "/run/current-system/services/~a" service))
      arguments)))

(defun kill-by-files (files signals)
  (let*
    ((files
       (loop for f in files
	     for rf := (ignore-errors (iolib/syscalls:realpath f))
	     when rf collect rf)))
    (loop
      for s in (or signals (list :term)) do
      (loop
	for rs in (list :cont s :cont)
	do
	(run-program-return-success
	  (uiop:run-program
	    `("fuser" ,@files "-k" "-s"
	      ,(format nil "-~a" (string-upcase (string rs)))))))
      do (sleep 0.2))
    (not (run-program-return-success
	   (uiop:run-program
	     `("fuser" ,@files))))))

(defun kill-by-log (logname &rest signals)
  (let*
    ((stdout-log
       (format
	 nil
	 "/var/log/system-lisp-logs/~a/stdout-freshest.log"
	 logname))
     (stderr-log
       (format
	 nil
	 "/var/log/system-lisp-logs/~a/stdout-freshest.log"
	 logname)))
    (kill-by-files (list stdout-log stderr-log) signals)
    ))

(defun kill-by-executable (exe &rest signals)
  (kill-by-files (list (which exe)) signals))

(defun kill-by-uid (uid &rest signals)
  (loop for s in (or signals (list :term)) do
        (loop for rs in (list :cont s :cont) do
              (run-program-return-success
                (uiop:run-program
                  `("pkill" "-U" ,uid
                    ,(format nil "-~a" (string-upcase (string rs))))))
              do (sleep 0.2)))
  (not
    (run-program-return-success
      (uiop:run-program
        `("pgrep" "-U" ,uid)))))
