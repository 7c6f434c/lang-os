{ pkgs, name, fontfile, fontsize ? 16, extraWidth ? 0, fontSetSize ? 512, symbolSets ? [] }:
let 
  fontsizeStr = builtins.toString fontsize;
in
pkgs.runCommand name {} ''
  "${pkgs.fontforge}/bin/fontforge" --lang=ff -c '
    Open($1);
    SelectAll();
    AutoHint();
    SetGasp(${fontsizeStr},1,65535,1);
    BitmapsAvail([${fontsizeStr}], 1);
    BitmapsRegen([${fontsizeStr}]);
    Generate("font.", "bdf");
  ' "${fontfile}"
  bdffile="font-${fontsizeStr}.bdf"
  width="$(grep AVERAGE_WIDTH "$bdffile" | cut -d ' ' -f 2)"
  oldwidth="$width"
  echo "AVERAGE_WIDTH was $width"
  width="$(( (((width - 1) / 10) + 1 + (${builtins.toString extraWidth})) * 10 ))"
  sed -i "$bdffile" -e "s/AVERAGE_WIDTH .*/AVERAGE_WIDTH $width/"
  echo "AVERAGE_WIDTH is $width"
  mkdir -p "$out/share/consolefonts"
  ${pkgs.bdf2psf}/bin/bdf2psf --fb "font-${fontsizeStr}.bdf" \
     "${pkgs.bdf2psf}/usr/share/bdf2psf/standard.equivalents" \
     "${pkgs.bdf2psf}/usr/share/bdf2psf/ascii.set+${pkgs.bdf2psf}/usr/share/bdf2psf/linux.set${
       pkgs.lib.concatMapStrings (x: "+${x}") symbolSets
     }+:${pkgs.bdf2psf}/usr/share/bdf2psf/useful.set" \
     ${builtins.toString fontSetSize} "$out/share/consolefonts/${name}.psf"
  echo "AVERAGE_WIDTH is $width (corrected from $oldwidth)"
  ${pkgs.gzip}/bin/gzip -k "$out/share/consolefonts/${name}.psf"
''
