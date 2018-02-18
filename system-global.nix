{
  pkgs ? import <nixpkgs> {}, systemEtc
}:
pkgs.runCommand "system-global" {} ''
  mkdir "$out"
  cd "$out"
  ln -s "${systemEtc}" etc
  mkdir -p bin usr/bin
  ln -s "${pkgs.bash}/bin/sh" "bin/sh"
  ln -s "${pkgs.coreutils}/bin/env" "usr/bin/env"
''
