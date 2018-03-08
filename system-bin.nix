{ 
  pkgs ? import <nixpkgs> {}, nixos ? import <nixpkgs/nixos>
  , nixosTools ? (import <nixpkgs/nixos/modules/installer/tools/tools.nix> 
                      {inherit pkgs; inherit(pkgs) lib; config={}; modulesPath = null;}).config.system.build
  , systemName ? "layered-system"
  , grub_efi ? pkgs.grub2_efi
  , setupScript ? ""
  , initScript ? "while true; do /bin/sh -i; done"
}:
let
  customSetup = pkgs.writeScript "custom-setup" setupScript;
  customInit = pkgs.writeScript "custom-init" initScript;
  _sinit = pkgs.sinit.override {
          rcshutdown = "/run/current-system/bin/back-to-initrd";
          rcreboot = "/run/current-system/bin/reinit";
          rcinit = "/run/current-system/bin/startup";
  };
in
pkgs.runCommand "system-bin" {} ''
  mkdir -p "$out"

  script () {
    name="$1"
    shift
    echo '#!${pkgs.stdenv.shell}' > "$out/$name"
    echo 'export basedir="$(dirname "$0")"' >> "$out/$name"
    echo 'if test -z "$targetSystem"; then targetSystem="$(readlink -f "$(dirname "$basedir")")"; fi' >> "$out/$name"
    for i in "$@"; do
      echo "$i" >> "$out/$name"
    done
    chmod a+x "$out/$name"
  }

  script activate '"$basedir/setup"'
  script switch '"$basedir/set-as-current"' '"$basedir/activate"' '"$basedir/boot"'
  script boot '"$basedir"/set-as-current' '"$basedir/assemble-grub-config" "$@"'
  script grub-print-header '"${./grub-print-header.sh}" "$@"'
  script assemble-grub-config 'export grub_print_header="$basedir/grub-print-header"' '"${./assemble-grub-config.sh}" "$@"'
  script install-efi-grub 'target="''${1:-/boot}"; shift' \
    '"${grub_efi}/bin/grub-install" --efi-directory="$target" --target=x86_64-efi "$@"' \
    'mkdir -p "$target/EFI/Boot"' \
    'cp "$target/EFI/grub/grubx64.efi" "$target/EFI/Boot/BootX64.EFI"' 

  script modprobe 'export PATH="${pkgs.coreutils}/bin:${pkgs.kmod}/bin"' \
    'export targetSystem' \
    'MODULE_DIR="$targetSystem/boot/kernel-modules/lib/modules" modprobe "$@" ||' \
    'MODULE_DIR="/run/booted-system/boot/kernel-modules/lib/modules" modprobe "$@" ||' \
    'false'
  script back-to-initrd '/bin/sh "${./back-to-initrd.sh}"'
  script start-shutdown 'test -n "$1" && ln -sfT "$1" /run/post-backpivot-command; kill -USR1 1'
  script poweroff '/bin/sh "/run/current-system/bin/start-shutdown" "${pkgs.writeScript "poweroff-f" "poweroff -f"}"'
  script reboot '/bin/sh "/run/current-system/bin/start-shutdown" "${pkgs.writeScript "reboot-f" "reboot -f"}"'

  script presetup '
    export PATH="$PATH:$targetSystem/sw/bin:${pkgs.coreutils}/bin"
    export PATH="$targetSystem/sw/bin:${pkgs.coreutils}/bin"

    echo "targetSystem: $targetSystem" >&2
    echo "PATH: $PATH" >&2

    /bin/sh "${./setup-pre-custom.sh}" "$targetSystem"

    "${customSetup}" "$targetSystem"

    ln -sfT /var/current-system/ /run/current-system
  '
  script setup '
    source "$targetSystem/bin/presetup"
    ln -sfT "$targetSystem" /var/current-system-new
    mv -fT /var/current-system-new /var/current-system
  '
  script init '
    "$targetSystem"/bin/setup
    export PATH="/var/current-system/sw/bin"
    ln -sfT "$targetSystem" /run/booted-system
    echo switch happened

    cd /run
    cp ${_sinit}/bin/sinit .

    exec ./sinit

    echo sinit failed

    while true; do bash -i || /bin/sh -i; done
  '
  script startup '"${customInit}" "$(dirname "$basedir")"'

  script system-passwd '${pkgs.shadow}/bin/passwd -R /var/auth "$@"'
  script system-groupadd '${pkgs.shadow}/bin/groupadd -R /var/auth "$@"'
  script system-groupdel '${pkgs.shadow}/bin/groupdel -R /var/auth "$@"'
  script system-groupmod '${pkgs.shadow}/bin/groupmod -R /var/auth "$@"'
  script system-useradd '${pkgs.shadow}/bin/useradd -R /var/auth "$@"'
  script system-userdel '${pkgs.shadow}/bin/userdel -R /var/auth "$@"'
  script system-usermod '${pkgs.shadow}/bin/usermod -R /var/auth "$@"'
  script system-chsh '${pkgs.shadow}/bin/chsh -R /var/auth "$@"'

  script use-nixos-prepare-root 'target="$1"; shift' \
    'echo "$basedir $targetSystem" >&2' \
    'pipe="$(mktemp -d "''${TMPDIR:-/tmp}/nix-export-pipe.XXXXXXXX")/system.closure"; mkfifo "$pipe"' \
    'nix copy "$targetSystem" "$@" --to "$target" --no-check-sigs' \
    'chroot "$target" "$targetSystem/bin/switch"'

  script push-closures-into-prepared-chroot \
    'target="$1"; shift' \
    'echo "Copying closures" >&2' \
    'nix-store -qR "$@" | xargs nix-store --export | chroot "$target" "/var/current-system/sw/bin/nix-store" --import > /dev/null'

  script set-as-current 'nix-env --set -p "/nix/var/nix/profiles/${systemName}" "$targetSystem"'

  script push-self-into-chroot \
    'target="$1"; shift' \
    'export PATH="$PATH:$targetSystem/sw/bin"' \
    'test -d "$target/nix/var/nix/db/" || "$targetSystem/bin/use-nixos-prepare-root" "$target"' \
    'test -d "$target/$targetSystem" || "$targetSystem/bin/push-closures-into-prepared-chroot" "$target" "$targetSystem"' \
    'chroot "$target" "$targetSystem/bin/set-as-current"' \
    'chroot "$target" "$targetSystem/bin/switch"'

  script update-self-from-expression 'targetSystem= "$(nix-build --no-out-link -A systemInstance "$@")/bin/switch"'

  script spawn-getty '"${./spawn-getty.sh}" "$@"'
  script use-console '"${./use-console.sh}" "$@"'
''
