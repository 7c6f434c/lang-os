{ pkgs ? import <nixpkgs> {}, nixos ? import <nixpkgs/nixos>}:
rec {
  fromNixOS = import ./use-from-nixos.nix {inherit pkgs nixos;};

  deeplinkAttrset = name: entries: (pkgs.runCommand name {} ''
    mkdir -p "$out"
    cd "$out"
    ${
    pkgs.lib.concatStrings (pkgs.lib.mapAttrsToList (k: v: ''mkdir -p "$(dirname "./${k}")"; '') entries)
    }
    ${
    pkgs.lib.concatStrings (pkgs.lib.mapAttrsToList (k: v: ''ln -sT "${v}" "./${k}"; '') entries)
    }
  '') // { inherit entries; };

  shadowAuth = deeplinkAttrset "shadow-auth" {
    "passwd" = "/var/auth/etc/passwd";
    "shadow" = "/var/auth/etc/shadow";
    "group" = "/var/auth/etc/group";
    "gshadow" = "/var/auth/etc/gshadow";
  };

  resolvConfLinks = pkgs.runCommand "resolv-conf-links" {} ''
    mkdir -p "$out"
    ln -s /var/etc/resolv.conf "$out"
    ln -s /var/etc/resolv.conf.dhclient "$out"
    ln -s /var/etc/resolv.conf.dhclient-new "$out"
  '';

  udevConf = {packages ? [], extraRules ? ""}: {
    systemd.package = pkgs.eudev;
    services.udev = {
      packages = [pkgs.fuse pkgs.libinput pkgs.lvm2 pkgs.android-udev-rules
        (pkgs.runCommand "eudev-lib-rules" {} ''
          mkdir -p "$out/etc/udev/rules.d/"
          cp -r "${pkgs.eudev}/var/lib/udev/rules.d"/{50-udev-default.rules,60-persistent-storage.rules} "$out/etc/udev/rules.d/"
          cp -r "${pkgs.eudev}/var/lib/udev/hwdb.d" "$out/etc/udev/"
          chmod -R u+rw "$out/etc/udev"
          sed -e 's@''${exec_prefix}@${pkgs.udev}@' -i "$out"/etc/udev/rules.d/*.rules
        '')
        (pkgs.runCommand "touchpad-rules" {} ''
          mkdir -p "$out/etc/udev/rules.d"
          cat > "$out"/etc/udev/rules.d/30-local-touchpad.rules <<EOF
            ACTION=="remove", GOTO="local_touchpad_end"
            SUBSYSTEM!="input", GOTO="local_touchpad_end"
            KERNEL!="event*", GOTO="local_touchpad_end"
            IMPORT{builtin}="input_id"
            ENV{ID_INPUT_TOUCHPAD}=="?*", SYMLINK+="input/touchpad%n"
            LABEL="local_touchpad_end"
          EOF
        '')
      ] ++ packages;
      inherit extraRules;
    };
    boot.hardwareScan = true;
    hardware.enableAllFirmware = true;
    networking.usePredictableInterfaceNames = false;
  };

  ianaEtc = deeplinkAttrset "iana-etc-entries" {
        protocols = "${pkgs.iana-etc}/etc/protocols";
        services = "${pkgs.iana-etc}/etc/services";
      };
  timeEtc = timezone: deeplinkAttrset "etc-time" {
        zoneinfo = "${pkgs.tzdata}/share/zoneinfo";
        localtime = "${pkgs.tzdata}/share/zoneinfo/${timezone}";
      };
  resolvEtc = deeplinkAttrset "etc-resolv"
        {
          "resolv.conf" = "/var/etc/resolv.conf";
          "resolv.conf.dhclient" = "/var/etc/resolv.conf.dhclient";
          "resolv.conf.dhclient-new" = "/var/etc/resolv.conf.dhclient-new";
        };
  mountEtc = deeplinkAttrset "etc-mount" {
        fstab = "/dev/null";
        mtab = "/proc/mounts";
        "fuse.conf" = pkgs.writeText "fuse.conf" ''
          user_allow_other
        '';
      };
  namesEtc = hostname: deeplinkAttrset "names-etc" (
     (fromNixOS.etcSelectComponents
             ["hostname" "hosts" "nsswitch.conf"]
             {
               networking.hostName="localhost";
               networking.hosts."127.0.0.1" = ["localhost4" "localhost"];
               networking.hosts."::1" = ["localhost6"];
             })
     // resolvEtc.entries // ianaEtc.entries //
     { "machine-id" = "/var/etc/machine-id"; }
     );
  authEtc = config: deeplinkAttrset "auth-etc" (
      (fromNixOS.etcSelectPrefix "pam.d/" config) //
      shadowAuth.entries //
      (fromNixOS.etcSelectComponents
        ["login.defs" "profile" "bashrc"] config) //
      {}
  );
}
