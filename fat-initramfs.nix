{
  pkgs ? (import <nixpkgs> {})
}:
pkgs.lib.makeExtensible (self: with self; {
  maybeCall = f: arg: if builtins.isFunction f then f arg else f;

  mountScript = "/bin/sh -i;";
  kernelPackages = pkgs: pkgs.linuxPackagesFor pkgs.linux_latest;
  udevPackages = pkgs: [];
  kernelModulePackages = kp: [];
  firmwarePackages = pkgs: [pkgs.firmwareLinuxNonfree];
  tools = pkgs: [];
  modprobeConfig = "";
  blacklistUdevRules = [];
  qemu = pkgs: pkgs.kvm;
  qemuArgs = "--enable-kvm -m 2G -bios ${pkgs.OVMF.fd}/FV/OVMF.fd";

  kernel = (maybeCall kernelPackages pkgs).kernel;
  bzImage = kernel + "/bzImage";

  firmwareSet = pkgs.buildEnv {
    name = "firmware-set";
    paths = maybeCall firmwarePackages pkgs;
    pathsToLink = ["/lib/firmware"];
  };
  _kernelPackages = maybeCall kernelPackages pkgs;
  _kernelModulePackages = [_kernelPackages.kernel] ++
    maybeCall kernelModulePackages _kernelPackages;
  kernelModules = pkgs.aggregateModules _kernelModulePackages;

  _udevPackages = maybeCall udevPackages pkgs ++ [
    pkgs.eudev pkgs.lvm2 pkgs.libinput
  ];

  udevRules = pkgs.runCommand "udev-rules" {} ''
    mkdir -p "$out"/etc/udev/{rules.d,hwdb.d}
    for i in ${toString _udevPackages}; do
      test -d "$i"/etc/udev/rules.d/ && ln -sf "$i"/etc/udev/rules.d/* "$out"/etc/udev/rules.d
      test -d "$i"/var/lib/udev/rules.d/ && ln -sf "$i"/var/lib/udev/rules.d/* "$out"/etc/udev/rules.d
      test -d "$i"/lib/udev/rules.d/ && ln -sf "$i"/lib/udev/rules.d/* "$out"/etc/udev/rules.d
      test -d "$i"/etc/udev/hwdb.d/ && ln -sf "$i"/etc/udev/hwdb.d/* "$out"/etc/udev/hwdb.d
      test -d "$i"/var/lib/udev/hwdb.d/ && ln -sf "$i"/var/lib/udev/hwdb.d/* "$out"/etc/udev/hwdb.d
      test -d "$i"/lib/udev/hwdb.d/ && ln -sf "$i"/lib/udev/hwdb.d/* "$out"/etc/udev/hwdb.d
    done
    for i in ${toString blacklistUdevRules}; do
      rm "$out/etc/udev/rules.d/$i" || true
    done
    ${pkgs.eudev}/bin/udevadm hwdb -u -r "$out"
  '';

  modprobeConf = pkgs.writeText "modprobe.conf" (''
    blacklist evbug
  '' + modprobeConfig);

  initTools = pkgs.buildEnv {
    name = "init-tools";
    paths = 
      (with pkgs; [
        (lowPrio busybox)
        kmod bashInteractive lvm2 cryptsetup coreutils
        gnugrep gnused eudev strace utillinux e2fsprogs
      ])
      ++ (maybeCall tools pkgs);
    ignoreCollisions = true;
    pathsToLink = ["/"];
    extraOutputsToInstall = ["bin" "out"];
  };

  initScript = ''
    set -x

    export PATH="/init-tools/bin"

    mkdir -p /new-root /proc /sys /dev /run /tmp /etc

    mount proc -t proc /proc

    mount sysfs -t sysfs /sys
    mount run-tmpfs -t tmpfs /run
    mount dev-tmpfs -t devtmpfs /dev

    mkdir -p /dev/shm /dev/pts /run/user /run/lock /run/nix
    
    mount shm -t tmpfs /dev/shm
    mount /devpts -t devpts /dev/pts

    chmod a+rwxt /dev/shm

    echo -n /init-tools/bin/modprobe > /proc/sys/kernel/modprobe
    echo -n /firmware > /sys/module/firmware_class/parameters/path

    ln -sfT /proc/mounts /etc/mtab
    touch /etc/fstab

    echo "Ready to start udev"

    modprobe dm-mod

    udevd &
    udevadm trigger --action=add
    udevadm settle

    vgchange -ay
   
    udevd &
    udevadm trigger --action=add
    udevadm settle

    for i in /dev/sd?; do hdparm -B 255 $i; done

    mount efivars -t pstore /sys/firmware/efi/efivars/

    mkdir /new-root

    test -z "$targetSystem" && export targetSystem="$(cat /proc/cmdline | tr ' ' '\n' | grep '^targetSystem=' | sed -e 's/^targetSystem=//')"
    test -z "$targetInit" && export targetInit="$(cat /proc/cmdline | tr ' ' '\n' | grep '^init=' | sed -e 's/^init=//')"
    test -z "$targetInit" && targetInit="$targetSystem/bin/init"

    cat /proc/cmdline | tr ' ' '\n' | grep 'debug_premount=1' && sh -i

    ${mountScript}

    cat /proc/cmdline | tr ' ' '\n' | grep 'debug_postmount=1' && sh -i

    cd /new-root/

    export PATH="$PATH:$targetSystem/sw/bin"
    chroot . test -e "$targetInit" || chroot . test -L "$targetInit" || {
      echo "Oops, target init is not found"
      sh -i
    }

    udevadm control -e

    lsmod

    mkdir -p ./{proc,sys,dev,run}
    mount --move /proc ./proc
    mount --move /sys ./sys
    mount --move /dev ./dev
    mount --move /run ./run

    mount -o bind,ro ./nix/store ./nix/store
    mount -o bind,remount,ro ./nix/store ./nix/store

    echo "Ready for switch_root"

    killall -s TERM
    sleep 0.1
    killall -s QUIT
    sleep 0.1
    killall -s KILL

    exec switch_root . ./"$targetInit"
  '';

  init = pkgs.writeScript "init" ("#!/bin/sh\n" + initScript);

  initrd = pkgs.makeInitrd {
    contents = [
      { object = init; symlink = "/init";}
      { object = kernelModules + "/lib/modules"; symlink = "/lib/modules"; }
      { object = initTools; symlink = "/init-tools"; }
      { object = pkgs.busybox; symlink = "/busybox"; }
      { object = initTools + "/bin/bash"; symlink = "/bin/sh"; }
      { object = initTools + "/bin/env"; symlink = "/usr/bin/env"; }
      { object = firmwareSet + "/lib/firmware"; symlink = "/firmware"; }
      { object = modprobeConf; symlink = "/etc/modprobe.d/modprobe.conf"; }
      { object = udevRules + "/etc/udev"; symlink = "/etc/udev"; }
    ];
  };

  qemuScript = pkgs.writeScript "qemu-script" ''#! /bin/sh
    ${maybeCall qemu pkgs}/bin/qemu-system-$(echo "${builtins.currentSystem}" | sed -e "s/-.*//") \
      ${qemuArgs} -kernel ${bzImage} -initrd ${initrd}/initrd "$@"
  '';
  qemuScriptTextMode = pkgs.writeScript "qemu-script" ''#! /bin/sh
    ${maybeCall qemu pkgs}/bin/qemu-system-$(echo "${builtins.currentSystem}" | sed -e "s/-.*//") \
      ${qemuArgs} -kernel ${bzImage} -initrd ${initrd}/initrd \
      -nographic -append "console=ttyS0" "$@"
  '';
})
