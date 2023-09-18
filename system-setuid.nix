{
  pkgs ? import <nixpkgs> {}, setuidPrograms
}:
let proglist = (if builtins.isFunction setuidPrograms then (setuidPrograms pkgs) else setuidPrograms); in
pkgs.runCommand "setuid-set" {} ''
    mkdir "$out"
    wrapper_dir="/var/setuid-wrapper-storage/$(basename "$out")"

    ${ pkgs.lib.concatMapStrings (x:
    ''
    ${pkgs.gcc}/bin/gcc -Wall -O2 -DWRAPPER_DIR="\"$wrapper_dir\"" -DSOURCE_PROG="\"${x.src}\"" ${<nixpkgs> + "/nixos/modules/security/wrappers/wrapper.c"} -o "$out/${x.name}" -L${pkgs.libcap.lib}/lib -I${pkgs.libcap.dev}/include -L${pkgs.libcap_ng}/lib -I${pkgs.libcap_ng}/include -lcap -lcap-ng
    ''
    ) proglist}

    echo "#!${pkgs.stdenv.shell}
      ${
        pkgs.lib.concatMapStrings (x:
          ''
            mkdir -p \"$wrapper_dir\"
            cp \"$out/${x.name}\" \"$wrapper_dir/${x.name}\"
            echo -n \"${x.src}\" >  \"$wrapper_dir/${x.name}.real\"
            chown ${x.owner or "0"}:${x.group or "0"} \"$wrapper_dir/${x.name}\"
            ${pkgs.lib.optionalString x.setuid or false ''chmod u+s \"$wrapper_dir/${x.name}\"''}
            ${pkgs.lib.optionalString x.setgid or false ''chmod g+s \"$wrapper_dir/${x.name}\"''}
            ${pkgs.lib.optionalString (x.setcap or null != null) ''${pkgs.libcap}/bin/setcap \"${x.setcap}\" \"$wrapper_dir/${x.name}\"''}
          ''
        ) proglist
      }
    " > "$out/install-script"
    chmod a+x "$out/install-script"

    ln -s "$wrapper_dir" "$out/wrappers"

  ''
