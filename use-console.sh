#! /bin/sh
    if test "$1" = "--"; then shift; fi
    if test -z "$1"; then 
	    echo -n "$(hostname) login: "
	    read user
	    shift
	    exec "$0" "$user" "$@"
    fi
    test -n "$CONSOLE_DEVICE" && chown "$1" "$CONSOLE_DEVICE"
    chown "$1" "$(tty)"
    mkdir -p /run/user/$(id -u "$1")
    chown "$1" -R /run/user/$(id -u "$1")
    chmod u+rwx /run/user/$(id -u "$1")
    mkdir -p /nix/var/nix/profiles/per-user/"$1"
    chown "$1" /nix/var/nix/profiles/per-user/"$1"
    chmod u+rwx /nix/var/nix/profiles/per-user/"$1"
    mkdir -p /nix/var/nix/gcroots/per-user/"$1"
    chown "$1" /nix/var/nix/gcroots/per-user/"$1"
    chmod u+rwx /nix/var/nix/gcroots/per-user/"$1"
    unset "CONSOLE_DEVICE"
    exec login "$1"
    #exec /run/current-system/sw/bin/su nobody -s /bin/sh -c "/run/wrappers/bin/su -l '$1' ${2:+-c '$2'}"
