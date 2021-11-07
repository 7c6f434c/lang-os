(import ./test-system.nix {}).extend ( self: super: {
  stage1 = super.stage1.extend (s1self: s1super: {
    kernelPackages = pkgs: pkgs.linuxPackagesFor pkgs.linux_latest;
    #kernelPackages = pkgs: pkgs.linuxPackagesFor pkgs.linux;

    mountScript = ''
      modprobe atkbd
      modprobe usbhid
      modprobe hid-generic
      modprobe hid-cherry
      modprobe mac-hid
      modprobe xhci-hcd
      modprobe ehci-hcd
      
      sh ${./mount-partitions-thinkpad.sh}
    '';
    modprobeConfig = ''
      blacklist nouveau
      blacklist iwlwifi
      
      ${builtins.readFile ./modprobe.conf}
    '';
  });

  stumpwmConfigDir = /home/raskin/src/lsp/stumpwm-config;
  stumpwmContrib = /home/repos/stumpwm-contrib;
  stumpwmLangOS = ../stumpwm;

  stumpwmWithConfig = self.pkgs.runCommand "stumpwm-with-config" {} ''
    mkdir -p "$out/bin"
    cp -r "${self.stumpwmContrib}" "contrib"
    chmod u+rwX -R contrib
    export HOME="$PWD"
    ${self.stumpwmWithDeps}/bin/stumpwm-lisp-launcher.sh \
      --eval '(require :stumpwm)' --eval '(in-package :stumpwm)' \
      --eval '(defvar stumpwm::*local-module-dir* "'"$PWD"'/contrib/")' \
      --eval '(defvar stumpwm::*langos* "${self.stumpwmLangOS}/")' \
      --load "${self.stumpwmConfigDir}"/setup.lisp \
      --load "${self.stumpwmConfigDir}"/all-defs.lisp \
      --eval '(sb-ext:save-lisp-and-die "'"$out"'/bin/stumpwm-with-config" 
        :executable t )'
    test -x "$out/bin/stumpwm-with-config"
  '';


  swPackages = super.swPackages ++ (with self.pkgs; [
    zsh python expect firmwareLinuxNonfree
    alsaUtils alsaTools mplayer rxvt_unicode mlterm
    androidenv.androidPkgs_9_0.platform-tools adb-sync
    powertop
    (self.pkgs.runCommand "local-keymap" {} ''
      mkdir -p "$out/share/keymaps/local/"
      cp ${./ru-en.map} "$out/share/keymaps/local/ru-en.map"
    '')
    self.stumpwmWithConfig
  ]);

  systemFonts = (import ./fonts.nix { inherit (self) pkgs; }).fonts;

  fontconfigConfPackages = [ (self.pkgs.hiPrio (self.pkgs.runCommand
    "fontconfig-kill-conf" {} ''
      mkdir -p "$out/etc/fonts/conf.d"
      mkdir -p "$out/etc/fonts/2.11/conf.d"
      for f in ; do
        for d in "$out/etc/fonts"/{,2.11}/conf.d/; do
          touch "$d/$f.conf"
        done
      done
    '')) ];

  nixOptions = super.nixOptions // {
    extraOptions = super.nixOptions.extraOptions or "" + ''
          gc-keep-outputs = true
          gc-keep-derivations = true
    '';
  };

  systemEtc = super.systemEtc.override (x: {
    paths = x.paths ++ [
      (super.etcPieces.deeplinkAttrset "etc-lvm" {
        "lvm" = "/var/etc/lvm";
      })
    ];
  });

  systemParts = super.systemParts // {
    services = self.etcPieces.deeplinkAttrset "systemServices"
    (super.systemParts.services.entries // {
      "from-nixos/gpm" = self.fromNixOS.serviceScript "gpm"
          { services.gpm.enable = true; services.gpm.protocol = "imps2"; };
    });
  };
  
  openglPackages = with self.pkgs; [ beignet vaapiIntel libvdpau-va-gl vaapiVdpau ];

  # pkgs = super.pkgs // {
  #   xorg = super.pkgs.xorg // {
  #     xorgserver = super.pkgs.xorg.xorgserver.overrideAttrs (x: {
  #       src = self.pkgs.fetchFromGitLab {
  #         domain = "gitlab.freedesktop.org";
  #         owner = "xorg";
  #         repo = "xserver";
  #         rev = "b3ae038c";
  #         hash = "sha256:18gnjyypy3qcqmxahc5rs82xhlrn606yf5jhz6jiy5c0ib3gcafw";
  #       };
  #       preConfigure = "";
  #       buildInputs = super.pkgs.xorg.xorgserver.buildInputs ++ [
  #         self.pkgs.autoreconfHook
  #         self.pkgs.xorg.utilmacros
  #         self.pkgs.xorg.fontutil
  #       ];
  #     });
  #   };
  # };
})
