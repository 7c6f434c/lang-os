{ 
  pkgs ? import <nixpkgs> {}
  , tty ? "tty12"
  , code ? ''(progn (format t "Hello from Common Lisp. Time is: ~s~%" (get-universal-time)) (sleep 10.1))''
  , deps ? []
}:
pkgs.writeScript "system-lisp-launcher" ''
    (
      ${
        pkgs.lib.concatMapStrings 
        (s: ''source "${s}/lib/common-lisp-settings"/*-path-config.sh;'')
        deps
      }
      NIX_LISP_ASDF_LOAD='(mapcar (function require) (list :asdf :sb-posix :sb-bsd-sockets))' NIX_LISP_PRELAUNCH_HOOK="nix_lisp_run_single_form '(load \"${pkgs.writeText "system-lisp-script.lisp" code}\")'" NIX_LISP_COMMAND="${pkgs.sbcl}/bin/sbcl" NIX_LISP_EARLY_OPTIONS="$NIX_LISP_EARLY_OPTIONS --noinform --dynamic-space-size 2048" ${pkgs.lispPackages.clwrapper}/bin/common-lisp.sh < /dev/null &>/dev/${tty}
    )
''
