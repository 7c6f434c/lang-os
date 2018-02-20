#! /bin/sh
touch ~/.user-lisp-shell_history
rlwrap -f . -c -e '' -b "'\"() " -q "\"" -C user-lisp-shell "$@"
