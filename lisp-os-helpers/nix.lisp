(defpackage :lisp-os-helpers/nix
  (:use :common-lisp :lisp-os-helpers/shell :lisp-os-helpers/util)
  (:export
    #:nix-expression
    #:nix-instantiate
    #:nix-eval
    #:nix-realise
    #:nix-build
    #:nix-shell
    #:nix-shell-for-packages
    ))
(in-package :lisp-os-helpers/nix)

(defun nix-expression
  (package-name &key nix-file 
                nixpkgs-suffix source-expression
		import-arguments)
  (let*
    ((expression
       (if (listp package-name)
         (format nil "[ ~{(~a) ~}]" package-name)
         package-name))
     (import-arguments (or import-arguments "{}"))
     (source-expression
       (or
         source-expression
         (and nixpkgs-suffix (format nil "with import <nixpkgs~a> ~a; " 
				     nixpkgs-suffix
				     import-arguments))
         (and nix-file
              (let*
                ((marker (random-number (expt 36 20))))
                (format
                  nil
                  "let
                      imported_~a = import ~a; 
                      true_~a = if (builtins.isFunction imported_~a) then 
                                   imported_~a ~a else imported_~a; 
                   in with true_~a; "
                   marker (if (probe-file nix-file)
                            (format nil "~s" (namestring (truename nix-file)))
                            nix-file)
                   marker     marker
                   marker import-arguments marker
                   marker)))
         (format nil "with import <nixpkgs> ~a; " import-arguments)))
     (full-expression (format nil "~a~a" source-expression expression)))
    full-expression))

(defun nix-instantiate-raw
  (name &key nix-file 
        nix-path nix-path-prefix nix-path-suffix
        nixpkgs-suffix source-expression 
	import-arguments
        nix-args extra-env
        )
  (let*
    ((nix-path-env (cl-ppcre:split ":" (uiop:getenv "NIX_PATH")))
     (nix-new-path
       (or nix-path 
           (and
             (or nix-path-prefix nix-path-suffix)
             (append
	       (alexandria:ensure-list nix-path-prefix)
	       nix-path-env
	       (alexandria:ensure-list nix-path-suffix)))))
     (expression
       (nix-expression
         name
         :nix-file nix-file
         :nixpkgs-suffix nixpkgs-suffix
         :source-expression source-expression
	 :import-arguments import-arguments))
     (nix-command
       `("nix-instantiate" "-E" ,expression ,@nix-args))
     (command
       (if nix-new-path
         (add-command-env
           nix-command `(("NIX_PATH" ,(format nil "~{~a~#[~:;:~]~}" nix-new-path))))
         nix-command))
     (command (if extra-env (add-command-env command extra-env) command))
     )
    (uiop:run-program
      command :output '(:string :stripped t)
      :error-output nil)))

(defun nix-instantiate (&rest args)
  (cl-ppcre:split *line-break-regexpr* (apply 'nix-instantiate-raw args)))

(defun nix-eval (expr &rest args &key nix-args)
  (apply
    'nix-instantiate-raw
    expr
    :nix-args (append nix-args (list "--eval-only"))
    args))

(defun nix-realise (derivations &key nix-args extra-env out-link error-output)
  (let*
    ((command `("nix-store" "-r"
                ,@(if out-link
                    `("--add-root" ,out-link "--indirect"))
                ,@(if (stringp derivations)
                    (list derivations) derivations) ,@nix-args))
     (command (add-command-env command extra-env)))
    (uiop:run-program command :output `(:string :stripped t)
		      :error-output error-output)))

(defun nix-build (name &rest args &key out-link nix-realise-error-output
                       verbose
		       &allow-other-keys)
  out-link
  (let*
    ((derivations
       (apply 'nix-instantiate name :allow-other-keys t args)))
    (when verbose
      (format *error-output* "Derivations: ~s~%" derivations))
    (apply 'nix-realise derivations :allow-other-keys t
	   :error-output nix-realise-error-output args)))

(defun nix-shell (command &rest arguments)
  (uiop:run-program
    `("nix-shell" "--run" ,(collapse-command command) ,@ arguments)))

(defun nix-shell-for-packages (command &rest arguments)
  (apply 'nix-shell command "-p" arguments))
