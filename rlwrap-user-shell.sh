#! /bin/sh
touch ~/.user-lisp-shell_history
history="$(mktemp ~/.user-lisp-shell_history.XXXXXXXX)"
cat ~/.user-lisp-shell_history > "$history"
rlwrap -D 2 -s 1000000 -f . -c -e '' -b "'\"() " -q "\"" -C user-lisp-shell -H "$history" "$@"
cat "$history" >> ~/.user-lisp-shell_history
cat ~/.user-lisp-shell_history | sort | uniq > ~/.user-lisp-shell_history.new
mv  ~/.user-lisp-shell_history.new  ~/.user-lisp-shell_history
rm "$history"
