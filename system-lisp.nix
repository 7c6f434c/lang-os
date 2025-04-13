{ 
  pkgs ? import <nixpkgs> {}
  , tty ? "tty12"
  , code ? ''(progn (format t "Hello from Common Lisp. Time is: ~s~%" (get-universal-time)) (sleep 10.1))''
  , deps ? []
}:
pkgs.writeScript "system-lisp-launcher" ''
    (
      export LANG=C.UTF-8
      export LOCALE_ARCHIVE=/var/current-system/sw/lib/locale/locale-archive
      ulimit -n 4096
      ulimit -n 65536
      ${pkgs.sbcl.withPackages (p: deps)}/bin/sbcl $NIX_LISP_EARLY_OPTIONS --noinform --dynamic-space-size 2048 --eval '(require :asdf)' --eval '(require :sb-posix)' --eval '(require :sb-bsd-sockets)' --load "${pkgs.writeText "system-lisp-script.lisp" code}" < /dev/null &>/dev/${tty}
    )
''
