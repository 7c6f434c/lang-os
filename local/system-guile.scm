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
              #f "echo Will run; echo ~a; ~a; read"
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

(sleep 10)
