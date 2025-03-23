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

    propagatedBuildInputs = [ system.lispOsHelpers ];

    dontBuild = true;
    dontStrip = true;

    ASDF_OUTPUT_TRANSLATIONS = "${builtins.storeDir}/:${builtins.storeDir}";
    USER_LISP_SHELL_RC = rcpath;

    installPhase = ''
      mkdir -p "$out/bin"
      ${pkgs.sbcl.withPackages (p: with p; [system.lispOsHelpers])}/bin/sbcl --eval '(require :asdf)' --eval '(ignore-errors
        (asdf:load-system :lisp-os-helpers)
        ${loadrc src}
        (format *error-output* "~s~%" (list *package* `lisp-shell-init))
        (funcall (find-symbol "BUILD-SHELL"
                    (find-package "LISP-OS-HELPERS/USER-ABBREVIATIONS")) 
            "'"$out"'/bin/user-lisp-shell")
      )'
     test -x "$out/bin/user-lisp-shell"
    '';
  };
}
