#! /bin/sh

    export CONSOLE_DEVICE="/dev/tty$1"
    chmod 0600 "$CONSOLE_DEVICE"
    setfacl -b "$CONSOLE_DEVICE"
    chown root "$CONSOLE_DEVICE"
    if test -n "$2"; then
	    export FAKE_SHELL="$2"
    fi
    setsid -w agetty tty$1 -8 -l /run/current-system/bin/use-console
    chmod 0600 "$CONSOLE_DEVICE"
    setfacl -b "$CONSOLE_DEVICE"
    chown root "$CONSOLE_DEVICE"
