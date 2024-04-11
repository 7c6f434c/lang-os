(defpackage :lisp-os-helpers/shell
  (:use :common-lisp)
  (:export
    #:escape-for-shell
    #:run-program-return-code
    #:run-program-return-success
    #:which
    #:program-output-lines
    #:*line-break-regexpr*
    #:add-command-env
    #:collapse-command
    #:file-lines
    #:overwrite-file
    #:grant-acl
    #:make-temporary-fifo
    #:wait-on-fifo
    #:add-command-fifo-fd
    #:wait-on-shell-command
    #:get-current-user-name
    #:masked-username
    ))

(in-package :lisp-os-helpers/shell)

(defvar *machine-id* nil)
(when (and (probe-file "/etc/machine-id") (not *machine-id*))
  (setf *machine-id*
        (alexandria:read-file-into-string "/etc/machine-id")))

(defun get-current-user-name ()
  (iolib/syscalls:getpwuid (iolib/syscalls:getuid)))

(defun escape-for-shell (s)
  (concatenate
    'string "'"
    (cl-ppcre:regex-replace-all "'" s "'\\\\''")
    "'"))

(defmacro run-program-return-code (&body body)
  `(multiple-value-bind
     (stdout stderr exit-code)
     (handler-bind
       ((uiop/run-program:subprocess-error (lambda (e) e (continue))))
       (progn
         ,@ body))
     stdout
     stderr
     exit-code))

(defmacro run-program-return-success (&body body)
  `(= 0 (run-program-return-code ,@body)))

(defun which (cmd)
  (uiop:run-program
    (list "which" cmd)
    :output '(:string :stripped t)))

(defparameter *line-break-regexpr*
  (format nil "(~a|~a|~a~a|~a~a)"
          #\Return
          #\Newline
          #\Return #\Newline
          #\Newline #\Return))

(defun program-output-lines (command &key
                                     ignore-error-status
                                     input-string input-file input-stdin)
  (multiple-value-bind
    (stdout stderr result)
    (with-input-from-string (s (or input-string ""))
      (uiop:run-program
        command
        :ignore-error-status ignore-error-status
        :output '(:string :stripped t)
        :input (cond
                 (input-file input-file)
                 (input-string s)
                 (input-stdin t)
                 (t nil))))
    stderr
    (values
      (cl-ppcre:split
        *line-break-regexpr*
        stdout)
      result)))

(defun file-lines (filename)
  (cl-ppcre:split
    *line-break-regexpr*
    (string-right-trim
      '(#\Newline #\Return)
      (alexandria:read-file-into-string filename))))

(defun overwrite-file (string filename)
  (with-open-file (f filename :direction :output
                     :external-format :utf-8
                     :if-exists :overwrite :element-type 'character)
    (format f "~a" string)))

(defun add-command-env (command env &key (env-helper "env") clear-env)
  (let*
    ((prefix
       `(,env-helper
          ,@(when clear-env `("-i"))
          ,@(loop
              for e in env
              for k :=
              (etypecase e
                (string e)
                (list (first e)))
              for v :=
              (etypecase e
                (string (uiop:getenv k))
                (list (second e)))
              when (null v) collect "-u"
              when (null v) collect k
              when v collect (format nil "~a=~a" k v))
          )))
    (etypecase command
      (string `(,@prefix "sh" "-c" ,command))
      (list `(,@prefix ,@command)))))

(defun collapse-command (command)
  (if (stringp command) command
    (format nil "~{~a ~}" (mapcar 'escape-for-shell command))))

(defun grant-acl (user file &key recursive (mode "rwX") (mask nil))
  (uiop:run-program
    `("setfacl" "-m" ,(format nil "u:~a:~a" user mode)
      ,@(when recursive `("-R"))
      ,@(if mask (mapcar 'namestring (directory file)) `(,file)))))

(defun make-temporary-fifo (&optional (mode #o600))
  (let*
    ((fifo-directory
       (format nil "/run/user/~a/lock-fifos/~a/"
               (iolib/syscalls:getuid) (iolib/syscalls:getpid)))
     (fifo-real-dir
       (progn
         (ensure-directories-exist fifo-directory)
         (uiop:run-program
           (list "mktemp" "-d" "-p" fifo-directory)
           :output '(:string :stripped t))))
     (fifo (format nil "~a/lock-fifo" fifo-real-dir)))
    (iolib/syscalls:mkfifo fifo mode)
    fifo))

(defun wait-on-fifo (fifo &key (remove t))
  (unwind-protect
    (with-open-file (f fifo)
      (read-char f nil nil))
    (when remove
      (ignore-errors
        (uiop:run-program
          (list "rm" "-rf"
                (directory-namestring fifo))))))
  t)

(defun add-command-fifo-fd (command fifo &optional (fd 14))
  (list
    "/bin/sh" "-c"
    (format nil "exec ~a<>~a; ~a"
            fd (escape-for-shell fifo)
            (collapse-command command))))

(defun multiplexer-spawn-command (command)
  (cond
    ((> (length (uiop:getenv "TMUX")) 0)
     `("tmux" "new-window" ,@command))
    ((> (length (uiop:getenv "STY")) 0)
     `("screen" "-X" "screen" ,@command))
    (t (error "Terminal multiplexer absent or not known"))))

(defun wait-on-shell-command (command
                               &key
                               (runner (lambda (command)
                                         (multiplexer-spawn-command command)))
                               run-options)
  (let*
    ((fifo (make-temporary-fifo))
     (thread (bordeaux-threads:make-thread
               (lambda () (wait-on-fifo fifo))
               :name "FIFO wait thread for shell command")))
    (unwind-protect
      (let*
        ((wrapped-command (add-command-fifo-fd command fifo))
         (runner-command (funcall runner wrapped-command)))
        (format t "~s~%" runner-command)
        (apply
          'uiop:run-program runner-command 
          run-options))
      (bordeaux-threads:join-thread thread))))

(defun masked-username (&rest additions)
  (subseq
    (format
      nil
      "~36r"
      (parse-integer
        (string-upcase
          (cl-ppcre:regex-replace
            " .*"
            (with-output-to-string (hash)
              (with-input-from-string
                (data (format nil "~a:~a~{::~a~}"
                              *machine-id* (get-current-user-name)
                              (remove nil additions)))
                (uiop:run-program (list "sha256sum") :input data :output hash)))
            ""))
        :radix 16))
    0 16))

