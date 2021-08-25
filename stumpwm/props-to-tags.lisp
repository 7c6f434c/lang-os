(in-package :stumpwm)

(unless (boundp '*property-notify-hook*)
  (defvar *property-notify-hook* ())
  (defvar *update-window-properties-function*
    (function update-window-properties))
  (defun update-window-properties (window atom)
    (run-hook-with-args *property-notify-hook* window atom)
    (funcall *update-window-properties-function* window atom)))

(defmacro defun-set-tags-by-condition (name arguments (w) condition)
  (let*
    ((tags (gensym)))
    `(defun ,name (,@arguments ,tags &key keep)
       (act-on-matching-windows
         (,w :screen)
         ,condition
         (unless keep (clear-tags nil ,w))
         (tag-window ,tags ,w)))))

(defun-set-tags-by-condition
  set-tags-by-hostname (hostname) (w)
  (window-hostname-p w hostname))

(defun-set-tags-by-condition
  set-tags-by-pid (pid) (w)
  (window-pid-p w pid))

(defun-set-tags-by-condition
  set-tags-by-prop-string (prop-string value) (w)
  (window-prop-string-p w prop-string value))

(defun-set-tags-by-condition
  set-tags-by-prop-int (prop-int value) (w)
  (window-prop-int-p w prop-int value))

(defun-set-tags-by-condition
  set-tags-by-hostname-and-pid (hostname pid) (w)
  (and
    (window-hostname-p w hostname)
    (window-pid-p w hostname)))

(defmacro defun-wait-set-tags-by-condition (name arguments (w) condition)
  (let*
    ((hook (gensym))
     (seen (gensym))
     (tags (gensym)))
    `(defun ,name (,@arguments ,tags &key keep forever)
       (let*
         ((,hook nil)
          (,seen nil))
         (setf 
           ,hook
           (lambda (,w &rest args)
             (declare (ignorable args))
             (ignore-errors
               (when ,condition
                 (unless keep (clear-tags nil ,w))
                 (tag-window ,tags ,w)
                 (setf ,seen t)
                 (unless forever
                   (setf
                     *property-notify-hook*
                     (remove ,hook *property-notify-hook*)))))
             (when (and forever ,seen)
               (unless
                 (act-on-matching-windows
                   (,w :screen)
                   (ignore-errors ,condition) t)
                 (setf *property-notify-hook*
                       (remove ,hook *property-notify-hook*))))))
         (push ,hook *property-notify-hook*)))))

(defun-wait-set-tags-by-condition
  wait-set-tags-by-hostname (hostname) (w)
  (window-hostname-p w hostname))

(defun-wait-set-tags-by-condition
  wait-set-tags-by-pid (pid) (w)
  (window-pid-p w pid))

(defun-wait-set-tags-by-condition
  wait-set-tags-by-hostname-and-pid (hostname pid) (w)
  (and
    (window-hostname-p w hostname)
    (window-pid-p w pid)))

(defcommand
  tag-application-forever-by-pid
  (tags &optional (window (current-window))) ((:rest "Tags to set: "))
  (let
    ((pid (window-pid window)))
    (set-tags-by-pid pid tags :keep t)
    (wait-set-tags-by-pid pid tags :keep t :forever t)))

(defcommand
  tag-task-forever-by-hostname
  (tags &optional (window (current-window))) ((:rest "Tags to set: "))
  (let
    ((hostname (window-hostname window)))
    (set-tags-by-hostname hostname tags :keep t)
    (wait-set-tags-by-hostname hostname tags :keep t :forever t)))

(defcommand
  tag-task-forever
  (tags &optional (window (current-window))) ((:rest "Tags to set: "))
  (let
    ((hostname (window-hostname window))
     (pid (window-pid window)))
    (set-tags-by-hostname-and-pid hostname pid tags :keep t)
    (wait-set-tags-by-hostname-and-pid hostname pid tags :keep t :forever t)))
