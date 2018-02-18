(asdf:defsystem
  :lisp-os-helpers
  :depends-on
  (
   :iolib :iolib/os :iolib/syscalls
   :uiop :iterate :cl-ppcre :bordeaux-threads :local-time alexandria :trivial-backtrace
   :clsql :clsql-sqlite3 :parenscript :drakma :cl-html-parse
   )
  :serial nil
  :components
  (
   (:file "util")
   (:file "shell")
   (:file "file-locks")
   (:file "vt" :depends-on ("shell" "file-locks"))
   (:file "references")
   (:file "fbterm-requests" :depends-on ("vt" "file-locks"))
   (:file "safe-read")
   (:file "auth-data" :depends-on ("shell"))
   (:file "timestamp")
   (:file "network" :depends-on ("shell"))
   (:file "daemon" :depends-on ("shell" "timestamp"))
   (:file "global-sqlite")
   (:file "nix" :depends-on ("shell"))
   (:file "unix-users")
   (:file "socket-command-server" :depends-on ("safe-read" "references" "auth-data" "fbterm-requests"))
   (:file "subuser" :depends-on ("shell" "timestamp" "global-sqlite" "util"))
   (:file "socket-command-client" :depends-on ("safe-read" "socket-command-server"))
   (:file "read-eval-print-once")
   (:file "user-abbreviations" :depends-on ("shell" "socket-command-client"))
   (:file "socket-command-definitions" :depends-on ("socket-command-server" "subuser" "daemon" "shell" "vt" "util"))
   (:file "subuser-x" :depends-on ("shell" "nix" "timestamp" "socket-command-client"))
   (:file "marionette" :depends-on ("subuser-x"))
   (:file "plain-web" :depends-on ())
   (:file "sound" :depends-on ("shell"))
   )
  )
