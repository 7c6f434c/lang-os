(asdf:defsystem
  :lisp-os-helpers
  :depends-on
  (
   ;; Apparently we need to make sure depedencies of dependencies are listed first
   :uiop
   :asdf
   :alexandria
   :iolib.common-lisp
   :iolib.base
   :cffi
   :cffi-grovel
   :uffi
   ;; The real dependencies
   :iolib :iolib/os :iolib/syscalls
   :uiop :iterate :cl-ppcre :bordeaux-threads :local-time alexandria :trivial-backtrace
   :clsql :clsql-sqlite3 :parenscript :drakma :cl-html-parse
   )
  :serial nil
  :components
  (
   (:file "util")
   (:file "shell")
   (:file "kernel" :depends-on ("shell"))
   (:file "file-locks")
   (:file "vt" :depends-on ("shell" "file-locks"))
   (:file "references")
   (:file "fbterm-requests" :depends-on ("vt" "file-locks"))
   (:file "safe-read" :depends-on ("util"))
   (:file "auth-data" :depends-on ("shell" "util"))
   (:file "timestamp")
   (:file "daemon" :depends-on ("shell" "timestamp"))
   (:file "network" :depends-on ("shell" "daemon"))
   (:file "global-sqlite")
   (:file "nix" :depends-on ("shell" "util"))
   (:file "unix-users")
   (:file "socket-command-server" :depends-on ("safe-read" "references" "auth-data" "fbterm-requests" "util"))
   (:file "subuser" :depends-on ("shell" "timestamp" "global-sqlite" "util" "unix-users"))
   (:file "socket-command-client" :depends-on ("safe-read" "socket-command-server" "util"))
   (:file "read-eval-print-once")
   (:file "ffi" :depends-on ())
   (:file "user-abbreviations" :depends-on ("shell" "socket-command-client" "ffi"))
   (:file "socket-command-definitions" :depends-on ("socket-command-server" "subuser" "daemon" "shell" "vt" "util" "kernel" "unix-users"))
   (:file "subuser-x" :depends-on ("shell" "nix" "timestamp" "socket-command-client"))
   (:file "marionette" :depends-on ("subuser-x" "util"))
   (:file "plain-web" :depends-on ())
   (:file "sound" :depends-on ("shell"))
   )
  )
