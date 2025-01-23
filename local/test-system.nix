{
  overrides ? x: {}
  , pkgs ? import <nixpkgs> {}
  , nixos ? import <nixpkgs/nixos>
}:
pkgs.lib.makeExtensible (self: with self; {
  inherit pkgs nixos;

  stage1 = (import ../fat-initramfs.nix {}).extend (s1self: s1super: {
    mountScript = ''
      modprobe atkbd
      sh ${./mount-partitions.sh}
    '';
    firmwarePackages = pkgs: [];
    modprobeConfig = builtins.readFile ./modprobe.conf;
    blacklistUdevRules = ["80-net-name-slot.rules"];
  });

  swPieces = import ../system-sw-pieces.nix { inherit (self) pkgs; };
 
  postgresql-package = self.pkgs.postgresql_13;
 
  lispOsHelpers = import ../lisp-os-helpers.nix {
    inherit (self) pkgs;
    src = "" + ../lisp-os-helpers;
    deps = with self.pkgs.sbcl.pkgs; [
      alexandria
      iolib iterate local-time cl-ppcre bordeaux-threads alexandria trivial-backtrace
      clsql clsql-sqlite3 parenscript drakma cl-html-parse
    ];
  };

  systemLispSettings = "/dev/null";

  systemLisp = import ../system-lisp.nix { 
          deps = with self.pkgs.sbcl.pkgs; [
            lispOsHelpers
          ];
          code = ''(defvar *lisp-os-helpers-package* "${lispOsHelpers}")
                   (load "${./user-info.lisp}")
                   (load "${systemLispSettings}")
                   (load "${./system-lisp.lisp}")'';
        };

  systemGerbil = import ../system-gerbil.nix { 
          deps = [];
	  code = ''
             (load "${./system-gerbil.scm}")
          '';
        };

  systemGuile = import ../system-guile.nix { 
          deps = [];
	  code = ''
             (load "${./system-guile.scm}")
          '';
        };

  loopStarter = command: self.pkgs.writeScript "loop-starter" ''
          trap : 1 2 3 13 14 15
          while true; do
                sleep 1
                ${command}
          done
  '';
 
  systemFonts = [];

  NixOSXConfig = {
             fonts.fonts = [];
             hardware.opengl = {
               enable = true;
               driSupport = true;
               driSupport32Bit = true;
             };
             services.xserver = {
               enableTCP = true;
               enable = true;
               exportConfiguration = true;
               inputClassSections = [
                   ''
                     Identifier "Keyboard catchall evdev"
                     MatchIsKeyboard "on"
                     Option "XkbRules" "base"
                     Option "XkbModel" "pc105"
                     Option "XkbLayout" "us"
                     Option "XkbOptions" "grp:caps_toggle, grp_led:caps, terminate:ctrl_alt_bksp"
                     Option "XkbVariant" ""
                     Driver "evdev"
                   ''
                   ''
                     Identifier "Mouse catchall evdev"
                     MatchIsPointer "on"
                     Driver "evdev"
                   ''
                   ''
                     Identifier "Touchscreen catchall evdev"
                     MatchIsTouchscreen "on"
                     Driver "evdev"
                   ''
                   ''
                     Identifier "Tablet catchall evdev"
                     MatchIsTablet "on"
                     Driver "evdev"
                   ''
                   ''
                     Identifier "Touchpad catchall evdev"
                     MatchIsTouchpad "on"
                     Driver "libinput"
                   ''
               ];
         
               layout = "us";			
               xkbOptions = "grp:caps_toggle, grp_led:caps, terminate:ctrl_alt_bksp";
               enableCtrlAltBackspace = true;
         
               libinput = {
                 enable = true;
		 scrollMethod = "edge";
               };
         
               deviceSection = ''
                 Option "DRI" "3"
               '';
               videoDrivers = self.pkgs.lib.mkForce ["modesetting"];
	     };
	};
  NixOSWithX = nixos {configuration = NixOSXConfig;};

  bindConfig = {
          services.bind = {
                  enable = true;
                  ipv4Only = false;
                  cacheNetworks = [
                          "127.0.0.0/24"
                                  "::1/128"
                                  "localhost"
                                  "192.168.0.0/16"
                  ];
                  zones = [
                  {
                          name = "local";
                          file = "" + ./local;
                          master = true;
                  }
                  {
                          name = ".";
                          file = "/var/lib/bind/root-servers";
                          master = true;
                  }
                  ];
                  extraConfig = ''
                          logging { category default { default_stderr; }; };
                  '';
                  extraOptions = ''
                          managed-keys-directory "/var/lib/bind/";
                  '';
          };
  };
  
  openglPackages = [];
  openglPackages32 = [];
  openglDriver = self.pkgs.buildEnv {
    name = "opengl-driver";
    paths = [ NixOSWithX.config.hardware.opengl.package ] ++
      openglPackages;
  };
  openglDriver32 = self.pkgs.buildEnv {
    name = "opengl-driver-32";
    paths = [ NixOSWithX.config.hardware.opengl.package32 ] ++
      openglPackages32;
  };

  sbcl-for-stumpwm = self.pkgs.sbcl_2_4_10;
  stumpwmWithDeps = 
  self.sbcl-for-stumpwm.pkgs.stumpwm.overrideLispAttrs (x: {
    lispLibs = x.lispLibs ++ 
      (with self.sbcl-for-stumpwm.pkgs; [ clx-truetype xkeyboard xembed ]);
  });

  swPackages = swPieces.corePackages ++ (with self.pkgs; [
        (hiPrio glibcLocales) xorg.libX11
        vim monotone screen xterm xorg.xprop
        sbcl asdf
        #gerbil
        guile
	postgresql-package
        nsjail
        stumpwmWithDeps
        unionfs-fuse
        xdummy pv mercurial fossil lvm2 rsync gawk ntp mtr host iotop syslogng
        btrfs-progs
        sway swaybg tunctl
        (swPieces.cProgram "vtlock" ../c/vtlock.c [] [])
        (swPieces.cProgram "file-lock" ../c/file-lock.c [] [])
        (swPieces.cProgram "in-pty" ../c/in-pty.c [] ["-lutil"])
        (swPieces.cProgram "numeric-su" ../c/numeric-su.c [] [])
        (swPieces.cProgram "simulate-setuid" ../c/simulate-setid.c [] [])
        lispOsHelpers
        (self.pkgs.runCommand "dejavu-fonts-sliced" {} ''
          mkdir -p "$out/share/fonts-sliced"
          cd "$out/share/fonts-sliced"
          for i in $(find "${self.pkgs.dejavu_fonts}" -name "*.ttf"); do
            mkdir "$(basename "$i" .ttf)"
            ln -s "$i" "$(basename "$i" .ttf)/"
          done
        '')
      ]) ++ (with stage1; [firmwareSet] ++ _kernelModulePackages)
      ++ systemFonts;

  systemParts = {
    bin = import ../system-bin.nix {
      initScript = ''
        ls -ld /nix/store
        ${loopStarter "/run/current-system/services/language-daemon/system-lisp"} &
        #${loopStarter "/run/current-system/services/language-daemon/system-gerbil"} &
        ${loopStarter "/run/current-system/services/language-daemon/system-guile"} &
        chvt 2
      '';
      setupScript = ''
        targetSystem="$1"
        mkdir -p /var/lib/cups /var/lib/ssh /var/lib/bind /var/empty
        ln -sf "/etc/ssh-config/sshd_config" /var/lib/ssh/
      '';
    };
    global = import ../system-global.nix {inherit systemEtc;};
    setuid = import ../system-setuid.nix {
      setuidPrograms = [
        { name = "su"; src="${self.pkgs.shadow.su}/bin/su"; setuid=true; }
        { name = "unix_chkpwd"; src="${self.pkgs.pam}/bin/unix_chkpwd"; setuid=true; }
        { name = "fusermount"; src="${self.pkgs.fuse}/bin/fusermount"; setuid=true; }
        { name = "fusermount3"; src="${self.pkgs.fuse3}/bin/fusermount3"; setuid=true; }
      ];
    };
    sw = self.pkgs.buildEnv rec {
      name = "system-path";
      paths = swPackages;
      extraOutputsToInstall = swPieces.allOutputNames paths;
      ignoreCollisions = true;
      pathsToLink = ["/"];
    };
    services = etcPieces.deeplinkAttrset "system-services" {
      "from-nixos/openssh" = fromNixOS.serviceScript "sshd"
        {services.openssh.enable = true;};
      "from-nixos/postgresql" = fromNixOS.serviceScript "postgresql"
        { services.postgresql.enable = true;
          services.postgresql.package = postgresql-package;
          services.postgresql.authentication = ''
            local all all peer
            host all all 127.0.0.1/32 md5
          '';
          services.postgresql.extraConfig = ''
            max_locks_per_transaction = 64
            shared_buffers = 131072
          '';
          };
      "from-nixos/cups" = fromNixOS.serviceScript "cups" {
         services.printing = {
           enable = true;
	   drivers = with self.pkgs; [
	     /* foo2zjs */ 
             foomatic-filters ghostscript cups-filters samba
             (gutenprint.overrideAttrs (x: {
               doCheck = false;
               nativeBuildInputs = x.nativeBuildInputs ++ [ perl ];
             }))
             /* hplip */
	   ];
         };
	 systemd.services.cups.serviceConfig.ExecStart = ''
	     ${self.pkgs.cups.out}/bin/cupsd -f
	   '';
      };
      "from-nixos/bind" = fromNixOS.serviceScript "bind" 
        (self.pkgs.lib.recursiveUpdate bindConfig {services.bind.configFile = "/var/etc/bind.conf";});
      "from-nixos/bind.conf" = (fromNixOS.nixosFun bindConfig).config.services.bind.configFile;
      "from-nixos/xorg" = self.pkgs.writeScript "xorg-start" ''
	    ln -Tfs "${openglDriver}" /run/opengl-driver
	    ln -Tfs "${openglDriver32}" /run/opengl-driver-32
	    ln -Tfs "${self.pkgs.libglvnd}" /run/libglvnd
	    ln -Tfs "${self.pkgs.pkgsi686Linux.libglvnd}" /run/libglvnd-32
	    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/run/opengl-driver/lib:/run/opengl-driver-32/lib:/run/libglvnd/lib:/run/libglvnd-32/lib"

	    ${self.pkgs.xorg.xorgserver}/bin/Xorg vt"$((7+''${1:-0}))" :"''${1:-0}" -logfile "/var/log/X.''${1:-0}.log" -config "${(fromNixOS.etcSelectComponent "X11/xorg.conf" NixOSXConfig)."X11/xorg.conf"}"
      '';
      "udevd" = self.pkgs.writeScript "udevd" ''
          ${self.pkgs.eudev}/bin/udevd &
          ${self.pkgs.eudev}/bin/udevadm trigger --action add
          ${self.pkgs.eudev}/bin/udevadm settle
        '';
      "nix-daemon" = self.pkgs.writeScript "nix-daemon" ''
        ${self.pkgs.nix}/bin/nix-daemon
      '';
      "dmesg-logger" = self.pkgs.writeScript "dmesg-logger" ''
        ${self.pkgs.util-linux}/bin/dmesg -w
      '';
      "language-daemon/system-lisp" = systemLisp;
      #"language-daemon/system-gerbil" = systemGerbil;
      "language-daemon/system-guile" = systemGuile;
    };
  };
  systemInstance = import ../system-instance.nix {
    inherit (self) pkgs;
    inherit stage1;
    inherit systemParts;
    kernelParameters = ["intel_pstate=disable"];
  };

  extraHostsEntries = {
    "${builtins.replaceStrings ["\n"] [""] (builtins.readFile ./vps-ip.private)}" = ["my-vm.local" "my-vm"];
  };

  etcPieces = (import ../system-etc-pieces.nix { inherit (self) pkgs nixos; }).extend (
  self_ep: super_ep: {
    inherit (self) extraHostsEntries;
  });

  inherit (etcPieces) fromNixOS;

  sessionVariables = {};

  fontConfigQuasiFonts = {};

  fontConfigQuasiFontPackage = pkgs.runCommand "fontconfig-quasifont-conf" {} ''
    mkdir -p "$out/etc/fonts/conf.d"
    ${
    pkgs.lib.concatStringsSep "\n"
    (pkgs.lib.mapAttrsToList
       (name: options: ''
         echo "<?xml version='1.0'?>
          <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
          <fontconfig> 
            <alias>
              <family>${name}</family>
              <prefer>
                <family>${(builtins.head options)}</family>
              </prefer>
              <accept>
                ${pkgs.lib.concatMapStringsSep "\n" (f: "<family>${f}</family>") (builtins.tail options)}
              </accept>
            </alias>
          </fontconfig>
         " >> "$out/etc/fonts/conf.d/00-quasifont-${name}.conf"
       '')
       fontConfigQuasiFonts
    )
    }
  '';

  fontconfigConfPackages = [ fontConfigQuasiFontPackage ];

  nixOptions = {
    useSandbox = true;
  };

  NixOSEtcFonts = (fromNixOS.etcSelectComponent "fonts" {
    fonts = {
      fonts = systemFonts;
      enableDefaultFonts = true;
      fontconfig = {
        enable = true;
        hinting.autohint = true;
        confPackages = fontconfigConfPackages;
      };
    };
  });

  gdFontPath = self.pkgs.runCommand "gd-font-path" {} ''
    mkdir -p "$out"
    ( find $(
      cat $(find -L "${self.NixOSEtcFonts.fonts}" -name "*.conf" -type f | tee /dev/stderr) | grep "[<]dir[>]" |
      sed -e 's@</dir>@&\n@g' |
      sed -e 's@<dir>@\n&@g' |
      grep "[<]dir[>]" |
      sed -e 's@<[^>]*>@@g' | grep . |
      sed -e 's@.*@&/lib/X11/fonts\n&/share/fonts@' | tee /dev/stderr
    ) -type d 2>/dev/null || true) |
    tr '\n' : | sed -e 's/:$//' > "$out/gd-font-path.conf"
  '';

  systemEtc = self.pkgs.buildEnv {
    name = "system-etc";
    paths = [
      (etcPieces.timeEtc "UTC")
      (etcPieces.namesEtc "localhost")
      (etcPieces.authEtc {
        security.pam.services = {
          sshd = {};
          cupsd = {};
          cups = {};
        };
        environment.sessionVariables = sessionVariables;
      })
      (etcPieces.deeplinkAttrset "etc-udev" 
        (fromNixOS.etcSelectPrefix "udev/" (etcPieces.udevConf {})))
      etcPieces.mountEtc
      (etcPieces.deeplinkAttrset "etc-ssl"
        {
          "ssl/certs/ca-bundle.crt" = "${self.pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          "ssl/certs/ca-certificates.crt" = "${self.pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          "pki/tls/certs/ca-bundle.crt" =  "${self.pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        })
      (etcPieces.deeplinkAttrset "etc-fonts"
       NixOSEtcFonts)
      gdFontPath
      (etcPieces.deeplinkAttrset "etc-nix"
        (fromNixOS.etcSelectPrefix "nix/" {
          nix = nixOptions;
        }))
      (etcPieces.deeplinkAttrset "etc-cups"
        {"cups" = "/var/lib/cups";})
      (etcPieces.deeplinkAttrset "etc-openssh"
        {"ssh" = "/var/lib/ssh";})
      (etcPieces.deeplinkAttrset "etc-openssh-config"
        (fromNixOS.etcSelectRenamed "ssh-config/sshd_config" "ssh/sshd_config"
          {services.openssh.enable = true;}))
      (etcPieces.deeplinkAttrset "etc-bind"
        {"bind" = "/var/lib/bind";})
      (etcPieces.deeplinkAttrset "etc-xorg"
        (fromNixOS.etcSelectComponent "X11/xorg.conf" NixOSXConfig))
    ];
    pathsToLink = ["/"];
  };

  #nixosTools = (import <nixpkgs/nixos/modules/installer/tools/tools.nix> 
  #  {inherit (self) pkgs; inherit (self.pkgs) lib; config={}; modulesPath = null;}).config.system.build;
})
