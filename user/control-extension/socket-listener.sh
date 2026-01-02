#! /bin/sh

        tgt="${BROWSER_CONTROL_SOCKET:-$HOME/browser-control-socket}"
        mkdir -p "$tgt"
"$(dirname "$0")"/../libexec/socket-listener.bin "$tgt/socket"

sleep 30

exit "$?"
