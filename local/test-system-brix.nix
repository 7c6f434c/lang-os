(import ./test-system.nix {}).extend ( self: super: {
  stage1 = super.stage1.extend (s1self: s1super: {
    mountScript = ''
      modprobe atkbd
      modprobe usbhid
      modprobe hid-generic
      modprobe mac-hid
      modprobe xhci-hcd
      modprobe ehci-hcd
      
      sh ${./mount-partitions-brix.sh}
    '';
    modprobeConfig = ''
      blacklist nouveau
      blacklist iwlwifi
      blacklist rtlwifi
      blacklist rtl8821ae
      
      ${builtins.readFile ./modprobe.conf}
    '';
  });

  swPackages = super.swPackages ++ (with self.pkgs; [
    zsh python xterm mlterm expect firmwareLinuxNonfree
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

          build-max-jobs = 8
          build-cores = 8
          max-jobs = 8
          cores = 8
    '';
  };

  systemEtc = super.systemEtc.override (x: {
    paths = x.paths ++ [
      (super.etcPieces.deeplinkAttrset "etc-lvm" {
        "lvm" = "/var/etc/lvm";
      })
    ];
  });

  systemLispSettings = ./system-lisp-settings-brix.lisp;
})
