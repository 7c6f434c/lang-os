{ 
  pkgs ? import <nixpkgs> {}
  , tty ? "tty12"
  , code ? ''(begin (display "Hello from Guile. Time is ") (display (car now)) (display "\n") (sleep 10.3))''
  , deps ? []
}:
pkgs.writeScript "system-guile-launcher" ''
    (
      ${
        pkgs.lib.concatMapStrings 
        (s: ''export GUILE_LOAD_PATH="$GUILE_LOAD_PATH''${GUILE_LOAD_PATH:+:}${s}"; '')
        deps
      }
      "${pkgs.guile}/bin/guile" -e '(load "${pkgs.writeText "system-guile-script.scm" code}")' < /dev/null &>/dev/${tty}
    )
''
