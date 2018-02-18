{
  pkgs ? import <nixpkgs> {}, setuidPrograms
}:
pkgs.runCommand "setuid-set" {} ''
    mkdir "$out"
    wrapper_dir="/var/setuid-wrapper-storage/$(basename "$out")"

    ${pkgs.gcc}/bin/gcc -Wall -O2 -DWRAPPER_DIR="\"$wrapper_dir\"" ${<nixpkgs> + "/nixos/modules/security/wrappers/wrapper.c"} -o "$out/canonical-wrapper" -L${pkgs.libcap.lib}/lib -I${pkgs.libcap.dev}/include -L${pkgs.libcap_ng}/lib -I${pkgs.libcap_ng}/include -lcap -lcap-ng

    echo "#!${pkgs.stdenv.shell}
      ${
        pkgs.lib.concatMapStrings (x:
          ''
            mkdir -p \"$wrapper_dir\"
            cp \"$out/canonical-wrapper\" \"$wrapper_dir/${x.name}\"
            echo -n \"${x.src}\" >  \"$wrapper_dir/${x.name}.real\"
            chown ${x.owner or "0"}:${x.group or "0"} \"$wrapper_dir/${x.name}\"
            ${pkgs.lib.optionalString x.setuid or false ''chmod u+s \"$wrapper_dir/${x.name}\"''}
            ${pkgs.lib.optionalString x.setgid or false ''chmod g+s \"$wrapper_dir/${x.name}\"''}
            ${pkgs.lib.optionalString (x.setcap or null != null) ''${pkgs.libcap}/bin/setcap \"${x.setcap}\" \"$wrapper_dir/${x.name}\"''}
          ''
        ) (if builtins.isFunction setuidPrograms then (setuidPrograms pkgs) else setuidPrograms)
      }
    " > "$out/install-script"
    chmod a+x "$out/install-script"

    ln -s "$wrapper_dir" "$out/wrappers"

  ''
