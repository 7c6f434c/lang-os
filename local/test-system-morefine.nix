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
      modprobe usb-storage
      modprobe uas
      
      sh ${./mount-partitions-morefine.sh}
    '';
    modprobeConfig = ''
      blacklist nouveau
      blacklist iwlwifi
      blacklist mt7921e
      
      ${builtins.readFile ./modprobe.conf}
    '';

    firmwarePackages = p: [ (p.runCommand "needed-firmware" {} ''
      mkdir -p "$out"
      ( cd "${p.linux-firmware}"; find . -type f ) | grep amdgpu | ( cd "$out" ; 
        while read f; do
          mkdir -p "$(dirname "$f")"
          cp "${p.linux-firmware}/$f" "$f"
        done
      )
    '') ];
  });

  swPackages = super.swPackages ++ (with self.pkgs; [
    zsh 
    (import ../marionette-python-packages.nix { inherit(self) pkgs; }).pypy27
    pypy3
    expect linux-firmware
    alsa-utils alsa-tools rxvt-unicode
    mlterm-wayland mlterm
    (mlterm.override 
     (x: {
      enableGuis = { 
      fb = true; 
      xlib = false;
      wayland = false;
      sdl2 = false;
      quartz = false;
      }; 
      desktopBinary = "true";
      }))
    kdePackages.konsole
    powertop
    man
    vulkan-tools clinfo
    libcap
    xorg.xauth xorg.xhost
  ]);

  systemFonts = (import ./fonts.nix { inherit (self) pkgs; }).fonts;

  fontconfigConfPackages = [ (self.pkgs.lib.hiPrio (self.pkgs.runCommand
    "fontconfig-kill-conf" {} ''
      mkdir -p "$out/etc/fonts/conf.d"
      mkdir -p "$out/etc/fonts/2.11/conf.d"
      for f in ; do
        for d in "$out/etc/fonts"/{,2.11}/conf.d/; do
          touch "$d/$f.conf"
        done
      done
    '')) ];

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
      "from-nixos/cups" = null;
    });
  };
  
  openglPackages = with self.pkgs; [ 
    libvdpau-va-gl libva-vdpau-driver
    mesa.opencl vulkan-loader libcap.lib
  ];

  systemLispSettings = ./system-lisp-settings-morefine.lisp;

  hostname = "morefine";

  nixOptions = super.nixOptions // {
    extraOptions = super.nixOptions.extraOptions or "" + ''
          gc-keep-outputs = false
          gc-keep-derivations = false

          build-max-jobs = 16
          build-cores = 16
          max-jobs = 16
          cores = 16

          secret-key-files = /nix/var/nix/cache-key.private
    '';
    settings.trusted-users = ["nix-builder"];
    settings.trusted-public-keys = [ "buildbox-morefine-s500plus-20250322:VYZrnhIxa7NvILYl2Lme0NfKbJrthYyhtGk5/D+O0LA=" "raskin-thinkpad-20250322:+rGdt5KlHiDySpO3fwuJtr3+MBw/C7QYvgcQrTA+1rY=" ];
  };

  grubTimeout = 3;

  kernelParameters = [ "ttm.pages_limit=15728640" ];

  setupScript = super.setupScript + ''
    ln -sf /var/current-system/graphics-drivers/* /run/
  '';
})

