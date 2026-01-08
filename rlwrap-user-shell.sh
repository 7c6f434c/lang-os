#! /bin/sh
if test -z "$NO_RLWRAP" && which rlwrap &> /dev/null; then
        touch ~/.user-lisp-shell_history
        history="$(mktemp ~/.user-lisp-shell_history.XXXXXXXX)"
        cat ~/.user-lisp-shell_history > "$history"
        rlwrap  -b "'\"() " -C user-lisp-shell -D 2 -s 1000000  -H "$history" -f . -c -e '' -q "\"" "$@"
        cat "$history" >> ~/.user-lisp-shell_history
        cat ~/.user-lisp-shell_history | sort | uniq > ~/.user-lisp-shell_history.new
        mv  ~/.user-lisp-shell_history.new  ~/.user-lisp-shell_history
        rm "$history"
else
        "$@"
fi
