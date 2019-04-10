#! /bin/sh
touch ~/.user-lisp-shell_history
history="$(mktemp ~/.user-lisp-shell_history.XXXXXXXX)"
cat ~/.user-lisp-shell_history > "$history"
rlwrap -C user-lisp-shell -D 2 -s 1000000  -H "$history" -f . -c -e '' -b "'\"() " -q "\"" "$@"
cat "$history" >> ~/.user-lisp-shell_history
cat "$history" >> ~/.user-lisp-shell_history_dump
cat ~/.user-lisp-shell_history | sort | uniq > ~/.user-lisp-shell_history.new
mv  ~/.user-lisp-shell_history.new  ~/.user-lisp-shell_history
rm "$history"
