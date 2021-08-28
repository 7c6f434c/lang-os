{
  system ? (import ../local/test-system.nix {})
  , src ? ./.
  , loadrc ? src: "(load \"${src}/user-shell.lisp\")"
  , rcpath ? "/dev/null"
}: rec {
  pkgs = system.pkgs;
  user-shell = pkgs.stdenv.mkDerivation {
    pname = "user-shell";
    version = "0.0.0";

    src = src;

    propagatedBuildInputs = [ system.lispOsHelpers 
      pkgs.lispPackages.clwrapper pkgs.lispPackages.clwrapper.lisp 
      pkgs.lispPackages.clwrapper.asdf ];

    dontBuild = true;
    dontStrip = true;

    ASDF_OUTPUT_TRANSLATIONS = "${builtins.storeDir}/:${builtins.storeDir}";
    USER_LISP_SHELL_RC = rcpath;

    installPhase = ''
      mkdir -p "$out/bin"
      common-lisp.sh --eval '(progn
        (asdf:load-system :lisp-os-helpers)
        ${loadrc src}
        (funcall (find-symbol "BUILD-SHELL"
                    (find-package "LISP-OS-HELPERS/USER-ABBREVIATIONS")) 
            "'"$out"'/bin/user-lisp-shell")
      )'
     test -x "$out/bin/user-lisp-shell"
    '';
  };
}
