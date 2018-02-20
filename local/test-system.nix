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

  swPieces = import ../system-sw-pieces.nix { inherit pkgs; };
 
  postgresql-package = pkgs.postgresql95;
 
  lispOsHelpers = import ../lisp-os-helpers.nix {
    inherit pkgs;
    src = "" + ../lisp-os-helpers;
    deps = with pkgs.lispPackages; [
      iolib iterate local-time cl-ppcre bordeaux-threads alexandria trivial-backtrace
      clsql clsql-sqlite3 parenscript drakma cl-html-parse
    ];
  };

  systemLisp = import ../system-lisp.nix { 
          deps = with pkgs.lispPackages; [
            lispOsHelpers
          ];
          code = ''(defvar *lisp-os-helpers-package* "${lispOsHelpers}")
                   (load "${./user-info.lisp}")
                   (load "${./system-lisp.lisp}")'';
        };

  systemGerbil = import ../system-gerbil.nix { 
          deps = [];
	  code = ''
             (load "${./system-gerbil.scm}")
          '';
        };

  loopStarter = command: pkgs.writeScript "loop-starter" ''
          trap : 1 2 3 13 14 15
          while true; do
                sleep 1
                ${command}
          done
  '';
  
  NixOSXConfig = {
             fonts.fonts = [];
             hardware.opengl.driSupport = true;
             hardware.opengl.driSupport32Bit = true;
             hardware.opengl.enable = true;
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
	     };
	};
  NixOSWithX = nixos {configuration = NixOSXConfig;};

  swPackages = swPieces.corePackages ++ (with pkgs; [
        glibcLocales
        vim monotone screen rxvt_unicode xorg.xprop
        sbcl lispPackages.clwrapper lispPackages.uiop asdf gerbil
	postgresql-package
        nsjail unionfs-fuse
        lispPackages.stumpwm alsaTools
        xdummy
        (swPieces.cProgram "vtlock" ../c/vtlock.c [] [])
        (swPieces.cProgram "file-lock" ../c/file-lock.c [] [])
        (swPieces.cProgram "in-pty" ../c/in-pty.c [] ["-lutil"])
        (swPieces.cProgram "numeric-su" ../c/numeric-su.c [] [])
        lispOsHelpers
      ]) ++ (with stage1; [firmwareSet] ++ _kernelModulePackages);

  systemParts = {
    bin = import ../system-bin.nix {
      initScript = ''
        ${loopStarter "/run/current-system/services/language-daemon/system-lisp"} &
        ${loopStarter "/run/current-system/services/language-daemon/system-gerbil"} &
        chvt 2
      '';
      setupScript = ''
        targetSystem="$1"
        mkdir -p /var/lib/cups /var/lib/ssh /var/lib/bind /var/empty
      '';
    };
    global = import ../system-global.nix {inherit systemEtc;};
    setuid = import ../system-setuid.nix {
      setuidPrograms = [
        { name = "su"; src="${pkgs.shadow.su}/bin/su"; setuid=true; }
        { name = "unix_chkpwd"; src="${pkgs.pam}/bin/unix_chkpwd.orig"; setuid=true; }
        { name = "fusermount"; src="${pkgs.fuse}/bin/fusermount"; setuid=true; }
        { name = "fusermount3"; src="${pkgs.fuse3}/bin/fusermount3"; setuid=true; }
      ];
    };
    sw = pkgs.buildEnv rec {
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
          services.postgresql.package = postgresql-package; };
      "from-nixos/cups" = fromNixOS.serviceScript "cups" {
         services.printing = {
           enable = true;
	   gutenprint = true;
	   drivers = with pkgs; [
	     foo2zjs foomatic_filters ghostscript cups_filters samba
             /* hplip */
	   ];
         };
	 systemd.services.cups.serviceConfig.ExecStart = ''
	     ${pkgs.cups.out}/bin/cupsd -f
	   '';
      };
      "from-nixos/bind" = fromNixOS.serviceScript "bind" {
        services.bind = {
	  enable = true;
	  ipv4Only = false;
	  cacheNetworks = [
	    "127.0.0.0/24"
	    "::1/128"
	    "localhost"
	    "192.168.0.0/16"
	  ];
	  zones = [];
	  extraConfig = ''
	    logging { category default { default_stderr; }; };
	  '';
	};
      };
      "from-nixos/xorg" = pkgs.writeScript "xorg-start" ''
	    ln -Tfs "${NixOSWithX.config.hardware.opengl.package or "/"}" /run/opengl-driver
	    ln -Tfs "${NixOSWithX.config.hardware.opengl.package32 or "/"}" /run/opengl-driver-32
	    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/run/opengl-driver/lib:/run/opengl-driver-32/lib"

	    ${pkgs.xorg.xorgserver}/bin/Xorg vt"$((7+''${1:-0}))" :"''${1:-0}" -logfile "/var/log/X.''${1:-0}.log" -config /etc/X11/xorg.conf
      '';
      "udevd" = pkgs.writeScript "udevd" ''
          ${pkgs.eudev}/bin/udevd &
          ${pkgs.eudev}/bin/udevadm trigger --action add
          ${pkgs.eudev}/bin/udevadm settle
        '';
      "nix-daemon" = pkgs.writeScript "nix-daemon" ''
        ${pkgs.nix}/bin/nix-daemon
      '';
      "language-daemon/system-lisp" = systemLisp;
      "language-daemon/system-gerbil" = systemGerbil;
    };
  };
  systemInstance = import ../system-instance.nix {
    inherit pkgs;
    inherit stage1;
    inherit systemParts;
    kernelParameters = ["intel_pstate=disable"];
  };

  etcPieces = import ../system-etc-pieces.nix { inherit pkgs nixos; };

  inherit (etcPieces) fromNixOS;

  sessionVariables = {};

  systemEtc = pkgs.buildEnv {
    name = "system-etc";
    paths = [
      (etcPieces.timeEtc "UTC")
      (etcPieces.namesEtc "localhost")
      (etcPieces.authEtc {
        security.pam.services = {
          sshd = {};
        };
        environment.sessionVariables = sessionVariables;
      })
      (etcPieces.deeplinkAttrset "etc-udev" 
        (fromNixOS.etcSelectPrefix "udev/" (etcPieces.udevConf {})))
      etcPieces.mountEtc
      (etcPieces.deeplinkAttrset "etc-ssl"
        {
          "ssl/certs/ca-bundle.crt" = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          "ssl/certs/ca-certificates.crt" = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          "pki/tls/certs/ca-bundle.crt" =  "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        })
      (etcPieces.deeplinkAttrset "etc-fonts"
       (fromNixOS.etcSelectComponent "fonts" {
          fonts = {
            enableDefaultFonts = true;
            fontconfig = {
              enable = true;
              hinting.autohint = true;
            };
          };
        }))
      (etcPieces.deeplinkAttrset "etc-nix"
        (fromNixOS.etcSelectPrefix "nix/" {
          nix = {
            useSandbox = true;
          };
        }))
      (etcPieces.deeplinkAttrset "etc-cups"
        {"cups" = "/var/lib/cups";})
      (etcPieces.deeplinkAttrset "etc-openssh"
        {"ssh" = "/var/lib/ssh";})
      (etcPieces.deeplinkAttrset "etc-bind"
        {"bind" = "/var/lib/bind";})
      (etcPieces.deeplinkAttrset "etc-xorg"
        (fromNixOS.etcSelectComponent "X11/xorg.conf" NixOSXConfig))
    ];
    pathsToLink = ["/"];
  };

  nixosTools = (import <nixpkgs/nixos/modules/installer/tools/tools.nix> 
    {inherit pkgs; config={}; modulesPath = null;}).config.system.build;
})
