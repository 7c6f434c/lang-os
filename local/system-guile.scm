(begin (display "Hello from Guile. Time is ") (display (car (gettimeofday))) (display "\n"))

(define (collapse-command command)
  (if
    (string? command) command
    (format
      #f "~{ '~a' ~}"
      (map
        (lambda (s)
          (regexp-substitute/global #f "'" s 'pre "'\\''" 'post))
        command))))

(define*
  (spawn-authorized-root-fbterm-command command #:key (fb-device "/dev/fb0") (vt 63))
  (system* "chvt" (format #f "~a" vt))
  (let*
    ((inner-command (collapse-command command))
     (su-command
       (collapse-command
         (list "/run/wrappers/bin/su" "-c"
               inner-command))))
    (system
      (format
        #f "~a < /dev/tty~a &> /dev/tty~a"
        (collapse-command
          (list
            "env" "--" (format #f "FRAMEBUFFER=~a" fb-device)
            "fbterm" "--font-size" "32" "--"
            "su" "nobody" "-s" "/bin/sh" "-c"
            (format
              #f "echo Will run; echo ~a; ~a; echo Done. Press Enter; read"
              inner-command su-command)))
        vt vt))))

(define (spawn-authorized-update expression . nix-path)
  (spawn-authorized-root-fbterm-command
    (list 
      "env" "--"
      (format #f "NIX_PATH=~{~a~#[~:;:~]~}" nix-path)
      "/run/current-system/bin/update-self-from-expression"
      expression)))

(define-once socket-command-handlers (make-hash-table))

(hash-set! socket-command-handlers "rebuild-from-path"
           (lambda*
             (path #:optional (nix-path '()))
             (apply 
               spawn-authorized-update
               path
               (if
                 (string? nix-path)
                 (map
                   (lambda (x) (array-ref x 0))
                   (list-matches "[^:]+" nix-path))
                 nix-path))))

(hash-set! socket-command-handlers "ping"
           (lambda* (#:optional value)
             (or value "alive")))

(hash-set! socket-command-handlers "quit"
           (lambda* ()
             (primitive-exit)))

(define socket-path "/run/system-guile-socket")
(define-once main-socket #f)

(define (read-or-f port)
  (catch
    'read-error
    (lambda () (read port))
    (lambda (key . args)
      #f)))

(define (no-symbols form)
  (cond
    ((symbol? form)
     (throw 'symbols-forbidden
            form))
    ((pair? form)
     (cons
       (no-symbols (car form))
       (no-symbols (cdr form))))
    (#t form)))

(define (eval-command command)
  (no-symbols command)
  (unless
    (and (list? command)
         (string? (car command)))
    (throw 'wrong-format command))
  (let*
    ((handler (hash-ref socket-command-handlers (car command))))
    (unless handler
      (throw 'unknown-command (car command)))
    (apply handler (cdr command))))

(if (not main-socket)
  (begin
    (set! main-socket (socket PF_UNIX SOCK_STREAM 0))
    (catch #t (lambda () (delete-file socket-path)) (lambda (key . args) #f))
    (bind main-socket AF_UNIX socket-path)
    (listen main-socket 3)
    (chmod socket-path #o666)
    (while 
      #t
      (let*
        ((connection (car (accept main-socket))))
        (begin-thread
          (setvbuf connection 'none)
          (catch
            #t
            (lambda ()
              (do
                ((command (read-or-f connection) (read-or-f connection)))
                ((or (not command) (eof-object? command)))
                (format #t "Command: ~s~%" command)
                (catch #t
                       (lambda ()
                         (no-symbols command)
                         (format
                           connection
                           "(\"value\" ~s)~%"
                           (eval-command command)))
                       (lambda (key . args)
                         (format connection "(\"error\" ~s)~%"
                                 (format #f "~s" (list key args))))))
              (close connection))
            (lambda (key . args)
              (catch
                #t
                (lambda () (close connection))
                (lambda (key . args) #f)))))
        ))))


