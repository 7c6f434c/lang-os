#!/bin/sh

targetSystem="$1";

    echo -n "$targetSystem"/bin/modprobe > /proc/sys/kernel/modprobe
    echo -n "$targetSystem"/sw/lib/firmware > /sys/module/firmware_class/parameters/path

    mkdir -p /var/auth/etc
    for i in passwd group shadow gshadow; do
            test -e /var/auth/etc/$i || cat /etc/$i > /var/auth/etc/$i
    done
    grep root /var/auth/etc/passwd >/dev/null || {
            echo "root:x:0:0:System administrator:/root:/run/current-system/sw/bin/bash" >> /var/auth/etc/passwd
            echo "sshd:x:1:65534:SSH privilege separation user:/var/empty:/run/current-system/sw/bin/nologin" >> /var/auth/etc/passwd
            echo "nobody:x:65534:65534:Unprivileged account:/var/empty:/run/current-system/sw/bin/nologin" >> /var/auth/etc/passwd
            echo "nixbld1:x:30001:30000:Nix build user 1:/var/empty:/run/current-system/sw/bin/nologin" >> /var/auth/etc/passwd
    }
    grep nixbld /var/auth/etc/group >/dev/null || {
            echo "root:x:0:" >> /var/auth/etc/group
            echo "wheel:x:1:" >> /var/auth/etc/group
            echo "users:x:100:" >> /var/auth/etc/group
            echo "nixbld:x:30000:nixbld1" >> /var/auth/etc/group
            echo "nogroup:x:65534:" >> /var/auth/etc/group
    }
    test -d /var/auth/"$targetSystem" || {
            mkdir -p /var/auth/nix/store
            mount --bind /nix/store /var/auth/nix/store
            mkdir -p /var/auth/home
            mount --bind /home /var/auth/home
            mkdir -p /var/auth/run
            mount --bind /run /var/auth/run
            mkdir -p /var/auth/var
            mount --bind /var /var/auth/var
            mkdir -p /var/auth/proc
            mount --bind /proc /var/auth/proc
            ln -sfT /run/current-system/global/bin /var/auth/bin
    }
    ln -sfT "$(readlink -f /etc/pam.d)" /var/auth/etc/pam.d

    mkdir -p /var/etc
    if ! test -e /var/etc/machine-id; then
            if test -e /etc/machine-id; then
                    cat /etc/machine-id > /var/etc/machine-id
            else
                    dd if=/dev/urandom bs=16 count=1 | od -t x1 -w16 -An | tr -d " " > /var/etc/machine-id
            fi
    fi

    (
      cd "$targetSystem/global"
      for i in *; do
        if ! ( test "$(readlink "/$i")" = "/var/current-system/global/$i" ); then
          mv "/$i" "/$i-$( date +%Y%m%d-%H%M%S )"
          ln -sfT "/var/current-system/global/$i" "/$i"
        fi
      done
    )

    "$targetSystem/setuid/install-script"
    mkdir -p "/run/wrappers"
    ln -sfT "$targetSystem/setuid/wrappers" /run/wrappers/bin
    ln -sfT "$targetSystem/setuid/wrappers" /var/setuid-wrapper

    ln -sf /var/current-system /nix/var/nix/gcroots/
    ln -sf /var/latest-booted-system /nix/var/nix/gcroots/

    mkdir -p /nix/var/nix/profiles/per-user/
    mkdir -p /nix/var/nix/profiles/per-user/root/channels
    mkdir -p /nix/var/nix/gcroots/per-user/

    "$targetSystem"/bin/modprobe af-packet
    ip link set lo up
    "$targetSystem"/bin/modprobe fuse

    bootedSystem="$(cat /proc/cmdline | tr " " "\\n" | grep targetSystem= | tr "=" "\\n" | tail -n 1)"

    ln -sfT "/var/latest-booted-system" /run/booted-system
    ln -sfT "$bootedSystem" /var/latest-booted-system

