{
  pkgs, stage1
  , systemParts
  , systemCaption ? "Layered-Nix-System"
  , kernelParameters ? []
}:
pkgs.runCommand (systemCaption + "-instance") {} ''
    mkdir -p "$out"/boot

    ${
      pkgs.lib.concatStrings (builtins.map (p: ''
        ln -s "${builtins.getAttr p systemParts}" "$out/${p}"
      '') (builtins.attrNames systemParts))
    }

    (
      cd "$out/boot"
      mkdir for-bootloader
      cd for-bootloader

      ln -s ${stage1.bzImage} "$out"/boot/for-bootloader/${builtins.baseNameOf stage1.kernel}.linux.efi
      ln -s ${stage1.initrd}/initrd "$out"/boot/for-bootloader/${builtins.baseNameOf stage1.initrd}.initrd.efi
      "${./grub-print-entry.sh}" "${systemCaption}" "${stage1.kernel}.linux.efi" "${stage1.initrd}.initrd.efi" \
          "BOOT_IMAGE=${stage1.kernel} targetSystem=$out ${builtins.toString kernelParameters}" > \
          "grub.part.cfg"
      echo '-kernel "${stage1.bzImage}" -initrd "${stage1.initrd}/initrd" -append "BOOT_IMAGE=${stage1.kernel} targetSystem='"$out"' ${builtins.toString kernelParameters}"' > "qemu-boot-parameters"

      ln -s ${stage1.kernelModules} "$out/boot/kernel-modules"
      ln -s ${stage1.initrd} "$out/boot/initrd-package"
      ln -s ${stage1.kernel} "$out/boot/kernel-package"
      ln -s ${stage1.kernel}/bzImage "$out/boot/kernel"
      ln -s ${stage1.initrd}/initrd "$out/boot/initrd"
    )
  ''
