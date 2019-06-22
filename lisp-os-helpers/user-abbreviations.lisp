(defpackage :lisp-os-helpers/user-abbreviations
  (:use
    :common-lisp
    :lisp-os-helpers/socket-command-client
    :lisp-os-helpers/shell
    )
  (:export
    #:! #:!! #:& #:&& #:>> #:$ #:$=
    #:editor #:periodically
    #:cd #:cwd #:ls #:ls* #:ps
    #:build-shell
    #:create-eval-socket
    #:>file
    ))
(in-package :lisp-os-helpers/user-abbreviations)

(defun build-shell (output)
  (setf cffi:*foreign-library-directories*
	(cffi::explode-path-environment-variable
	  "NIX_LISP_LD_LIBRARY_PATH"))
  (loop
    with libpath :=
    (uiop:split-string
      (uiop:getenv "NIX_LISP_LD_LIBRARY_PATH")
      :separator ":")
    for l in sb-alien::*shared-objects*
    for ns := (sb-alien::shared-object-namestring l)
    do (and (> (length ns) 0) (not (equal (elt ns 0) "/"))
	    (let*
	      ((prefix (find-if
			 (lambda (s)
			   (probe-file (format nil "~a/~a" s ns)))
			 libpath))
	       (fullpath (and prefix (format nil "~a/~a" prefix ns))))
	      (when fullpath
		(setf
		  (sb-alien::shared-object-namestring l) fullpath
		  (sb-alien::shared-object-pathname l)
		  (probe-file fullpath))))))
  (sb-ext:save-lisp-and-die output :executable t))

(eval-when
  (:compile-toplevel :load-toplevel :execute)
  (defun underscore-to-capitals (s &key invert)
    (apply
      'concatenate 'string
      (loop
	for c across s
	for pending-this := nil then pending-next
	for pending-next := (and (not pending-this) (equal c #\^))
	if (not pending-next) collect (if (equal invert pending-this)
		  (string-downcase c) (string-upcase c)))))
  (defun to-string (e)
    (etypecase e
      (string e)
      (null "")
      (keyword (underscore-to-capitals (symbol-name e) :invert t))
      (symbol (underscore-to-capitals (symbol-name e) :invert nil))
      (pathname (namestring e))
      (number (format nil "~a" e))))
  (defun run-program-argument-code (s)
    (cond
      ((null s) s)
      ((and (symbolp s) (equal (elt (symbol-name s) 0) #\/))
       (underscore-to-capitals (symbol-name s)))
      ((and (listp s) (equal (first s) :string)
	    (symbolp (second s)))
       `',s)
      ((and (listp s) (equal (first s) :string))
       `(make-string-input-stream ,(second s)))
      ((keywordp s) s)
      ((pathnamep s) s)
      (t s)))
  (defun to-environment-string (e)
    (cond
      ((listp e) `(string ,e))
      (t (to-string e))
      ))
  (defun split-command (l)
    (let*
      ((command nil) (arguments nil) (environment nil))
      (loop
	for e in l
	for lt on l
	for parameter := nil then parameter-next
	for parameter-next := (and (not parameter) (keywordp e))
	for environment-entry := nil then environment-next
	for environment-next := (equal e :=)
	do
	(if parameter
	  (if environment-entry
	    (push (if (listp e)
		    (cons 'list (mapcar 'to-environment-string e))
		    (to-environment-string e))
		  environment)
	    (push (run-program-argument-code e) arguments))
	  (etypecase e
	    (keyword (case e
		       ((:>) (push :output arguments))
		       ((:2>) (push :error-output arguments))
		       ((:<) (push :input arguments))
		       ((:&>) 
			(push :output arguments)
			(push (run-program-argument-code (second lt)) arguments)
			(push ::error-output arguments))
		       (:=)
		       (t (push e arguments))))
	    ((or string symbol number pathname null)
	     (push (to-string e) command))
	    (list
	      (cond
		((eq :splice (first e))
		 (multiple-value-bind
		   (scommand sarguments)
		   (split-command (rest e))
		   (setf command (append (reverse scommand) command)
			 arguments (apply (reverse sarguments) arguments))))
		((keywordp (first e))
		 (multiple-value-bind
		   (scommand sarguments)
		   (split-command e)
		   (setf command (append (reverse scommand) command)
			 arguments (apply (reverse sarguments) arguments))))
		(t (push e command)))))))
      (values (reverse command) (reverse arguments) (reverse environment)))))

(defmacro ! (&rest data)
  (multiple-value-bind
    (command arguments environment) (split-command data)
    `(uiop:run-program
       ,(if environment `(add-command-env (list ,@command) (list ,@environment))
	  `(list ,@command))
       :ignore-error-status t ,@arguments
       :output t :error-output t :input t :pty nil)))

(defmacro & (&rest data)
  (multiple-value-bind
    (command arguments) (split-command data)
    `(uiop:launch-program (list ,@command) ,@arguments)))

(defmacro !! (&rest data)
  `(! screen -^X screen ,@data :&> nil :< nil))

(defmacro
  >>-impl (bangs &key stream)
  (cond
    ((null bangs))
    ((not (listp (first bangs)))
     `(>>-impl ,(cdr bangs) :stream ,(first bangs)))
    ((= (length bangs) 1)
     `(! :< ,stream ,@(first bangs)))
    (t
      (let ((first-process (gensym)))
	`(let
	   ((,first-process
	      (& :< ,stream :> :stream
		 ,@(first bangs))))
	   (>>-impl
	     ,(cdr bangs) :stream
	     (uiop:process-info-output ,first-process)))))))

(defmacro >> (&rest bangs)
  `(>>-impl ,bangs :stream *standard-input*))

(defmacro $ (&rest args)
  (cond
    ((= 1 (length args)) `(uiop:getenv ,(to-environment-string (first args))))
    ((and (symbolp (second args)) 
	  (equal ">>" (symbol-name (second args))) 
	  (null (first args)))
     (let
       ((stream (gensym)))
       `(cl-ppcre:split 
	  *line-break-regexpr*
	  (with-output-to-string (,stream)
	    (>> ,(cddr args) :stream ,stream)))))
    ((and (second args) (null (first args)))
     (let
       ((stream (gensym)))
       `(cl-ppcre:split 
	  *line-break-regexpr*
	  (with-output-to-string (,stream)
	    (! :> ,stream ,@(cdr args))))))))

(defmacro $= (name value)
  `(setf (uiop:getenv ,(to-environment-string name))
	 ,(to-environment-string value)))

(defun editor (&optional filename)
  (when (or ($ :visual) ($ :editor))
    (!! (or ($ :visual) ($ :editor)) (identity filename))
    t))

(defun cd (&optional dir)
  (uiop:chdir (or dir ($ "HOME")))
  (setf *default-pathname-defaults* (pathname (uiop:getcwd))))

(defun cwd () (namestring (uiop:getcwd)))

(defun ls (&optional name &rest args)
  (mapcar
    'to-string
    (apply
      'uiop:directory*
      (to-string (or name (uiop:getcwd))) args)))

(defun ls* (&optional name)
  (list
    (mapcar 'to-string
            (uiop:directory-files (to-string (or name (uiop:getcwd)))))
    (mapcar 'to-string
            (uiop:subdirectories (to-string (or name (uiop:getcwd)))))))

(defmacro && (&rest args)
  `(bordeaux-threads:make-thread
     (lambda () ,@args)
     :name "Background worker thread"))

(defun ps ()
  (let*
    ((raw ($ () "ps" "-A" "-ww" "-o" "pid:8,uid:8,etimes:12,user:40,cmd" "--no-headers"))
     (split (loop for s in raw collect (cl-ppcre:split " +" s :limit 6)))
     (marked (loop for l in split collect 
                   (loop 
                     for e in (rest l)
                     for k in `(:pid :uid :elapsed :user :command)
                     collect k collect e))))
    marked))

(defmacro periodically (period &body body)
  `(loop
    do (format t "Results:~%~{~s~%~}" (list ,@body))
    do (format t "~s~%~%" (local-time:now))
    do (sleep ,period)))

(defun create-eval-socket ()
  (let*
    ((socket-name (format nil "/run/user/~a/user-lisp-evaluator/socket"
                          (iolib/syscalls:getuid))))
    (ensure-directories-exist socket-name)
    (iolib/syscalls:chmod (directory-namestring socket-name) #o0700)
    (unless
      (ignore-errors
        (let*
          ((s (iolib:make-socket :connect :active
                                 :address-family :local
                                 :type :stream
                                 :remote-filename socket-name)))
          (unwind-protect
            (progn
              (format s "1~%")
              (finish-output s)
              (equal (read-line s nil nil) "1"))
            (close s))))
      (bordeaux-threads:make-thread
        (lambda ()
          (ignore-errors
            (let*
              ((socket
                 (iolib:make-socket
                   :connect :passive
                   :address-family :local
                   :type :stream
                   :local-filename socket-name
                   :external-format :utf-8)))
              (loop
                for client-socket :=
                (ignore-errors (iolib:accept-connection socket :wait t))
                do
                (ignore-errors
                  (bordeaux-threads:make-thread
                    (lambda ()
                      (ignore-errors
                        (unwind-protect
                          (loop
                            for form := (read client-socket nil nil)
                            while form
                            do (format client-socket "~s~%"
                                       (eval
                                         `(let
                                            ((client-socket ,client-socket))
                                            client-socket
                                            ,form)))
                            do (finish-output client-socket)
                            do (sleep 0.05))
                          (ignore-errors (close client-socket)))))
                    :name "User socket evaluator connection handler")))
              )))
        :name "User socket evaluator"))))

(defun >file (string)
  (uiop:with-temporary-file
    (:pathname p :stream s :keep t)
    (write string :stream s)
    p))
