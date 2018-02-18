(defpackage :lisp-os-helpers/marionette
  (:use
    :common-lisp
    :lisp-os-helpers/subuser-x
    )
  (:export
    #:subuser-firefox
    #:with-marionette
    #:with-new-firefox-marionette
    #:ask-marionette
    #:ask-marionette-parenscript
    #:marionette-set-pref
    ))
(in-package :lisp-os-helpers/marionette)

(defvar *ambient-marionette-socket* nil)

(defmacro with-marionette ((socket-path 
                             &key window (context :content)
                             (socket-name '*ambient-marionette-socket*))
                           &rest code)
  `(let*
     ((,socket-name
        (iolib:make-socket
          :connect :active :address-family :local :type :stream
          :remote-filename ,socket-path
	  :external-format :utf-8)))
     ,@(when window
         `((format ,socket-name "session.switch_to_window('~a')~%" ,window)
           (finish-output ,socket-name)
           (read-line ,socket-name)))
     ,@(when context
         `((format ,socket-name "session.set_context(session.CONTEXT_~a)~%"
                   (string-upcase ,context))
           (finish-output ,socket-name)
           (read-line ,socket-name)))
     ,@ code))

(defun skip-marionette-messages (&key (socket *ambient-marionette-socket*))
  (let*
    ((key (format nil "~36r" (random (expt 36 20)))))
    (format socket "'~a'~%" key)
    (finish-output socket)
    (loop
      for line := (read-line socket nil nil)
      unless line collect :eof
      while line
      until (equal line key)
      collect line)))

(defun ask-marionette (code &key (socket *ambient-marionette-socket*))
  (skip-marionette-messages :socket socket)
  (format socket "~a~%" code)
  (finish-output socket)
  (skip-marionette-messages :socket socket))

(defun escape-for-python (str)
  (let*
    ((str str)
     (str (cl-ppcre:regex-replace-all "\\\\" str "\\\\\\\\"))
     (str (cl-ppcre:regex-replace-all "[\"']" str "\\\\\\1"))
     (str (cl-ppcre:regex-replace-all (string #\Return) str "\\\\r"))
     (str (cl-ppcre:regex-replace-all (string #\Newline) str "\\\\n"))
     (str (cl-ppcre:regex-replace-all (string #\Null) str "\\\\0"))
     )
    str))

(defun ask-marionette-parenscript (ps-code 
                                    &key
                                    (socket *ambient-marionette-socket*)
                                    context)
  (let*
    ((js-code (ps:ps* ps-code))
     (js-code-escaped (escape-for-python js-code))
     (py-code
       (format nil "session.execute_script(\"~a\")"
	       js-code-escaped)))
    (when context
      (ask-marionette 
        (format
          nil "session.set_context(session.CONTEXT_~a)" (string-upcase context))
        :socket socket))
    (ask-marionette py-code :socket socket)))

(defun marionette-set-pref (key value &key (socket *ambient-marionette-socket*))
  (ask-marionette
    (format nil "session.set_pref('~a',~a)"
            (escape-for-python key)
            (firefox-pref-value-js value))
    :socket socket))

(defmacro with-new-firefox-marionette ((&rest marionette-args)
                                       (arguments &rest firefox-args)
                                       &rest body)
  (let*
    ((marionette-socket-name (gensym))
     (marionette-socket-path (gensym))
     (thread (gensym)))
    `(let*
       ((,marionette-socket-name (subuser-name-and-marionette-socket
                                   ,@ firefox-args :allow-other-keys t))
        (,marionette-socket-path (getf ,marionette-socket-name
                                       :marionette-socket))
       (,thread (bordeaux-threads:make-thread
                  (lambda ()
                    (apply
                      'subuser-firefox
                      ,arguments
                      :allow-other-keys t
                      (append
                        ,marionette-socket-name
                        (list ,@ firefox-args))))
                  :name "Firefox launcher thread")))
       (loop
         for k from 1 to 10
         do (sleep 1)
         when (probe-file ,marionette-socket-path) return nil
         finally (error "socket creation timeout"))
       (format t "Socket: ~s~%" ,marionette-socket-path)
       (loop
         for k from 1 to 6
         for test-thread :=
         (bordeaux-threads:make-thread
           (lambda ()
             (let*
               ((response
                  (ignore-errors
                    (with-marionette
                      (,marionette-socket-path)
                      (ask-marionette "1")))))
               (unless
                 (equal (list "1") response)
                 (format *error-output* "Response: ~s~%" response)
                 (sleep 100))))
           :name "Marionette test thread")
         do (sleep 3)
         do (if (bordeaux-threads:thread-alive-p test-thread)
              (bordeaux-threads:destroy-thread test-thread)
              (return nil))
         finally (error "Marionette does not respond at ~s"
                        ,marionette-socket-path))
       (prog1
         (with-marionette
           (,marionette-socket-path ,@ marionette-args)
           ,@body)
         (bordeaux-threads:join-thread ,thread)))))
