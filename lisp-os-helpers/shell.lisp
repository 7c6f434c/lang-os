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
    ))

(in-package :lisp-os-helpers/shell)

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

(defun add-command-env (command env &key (env-helper "env"))
  (let*
    ((prefix
       `(,env-helper
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
