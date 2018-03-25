#!/bin/sh
        boot_dir="${1:-/boot}"

      mkdir -p "$boot_dir"/kernels
      grubHeader="$("$grub_print_header" "$boot_dir")"
      cp -f "$boot_dir"/grub/grub.cfg{,.old}
      sync -f "$boot_dir/grub/grub.cfg.old"

      mkdir -p "$boot_dir"/grub/fragments.new
      for i in /nix/var/nix/profiles/*/ /run/booted-system/ /var/current-system/; do
        test -e "$i/boot/for-bootloader/grub.part.cfg" && {
          test -d "$boot_dir/grub/fragments/$(basename "$i")" &&
          basename "$i" | grep '[-][0-9]*-link$' >/dev/null &&
            mv "$boot_dir/grub/fragments/$(basename "$i")" "$boot_dir/grub/fragments.new/$(basename "$i")"
        }
      done
      sync -f "$boot_dir/grub"
      rm -rf "$boot_dir"/grub/fragments
      mv "$boot_dir"/grub/fragments.new "$boot_dir"/grub/fragments
      sync -f "$boot_dir/grub"
      for i in "$boot_dir"/kernels/*.efi; do
        grep "${i#$boot_dir}" "$boot_dir"/grub/fragments/*/grub.part.cfg -m1 > /dev/null || rm "$i"
      done
      n=0
      rm "$boot_dir"/grub/fragment-index/*
      mkdir -p "$boot_dir"/grub/fragment-index/
      for i in /var/current-system/ /run/booted-system/ /nix/var/nix/profiles/*-link/ ; do
        test -e "$i/boot/for-bootloader/grub.part.cfg" && {
          n=$((n+1))
          echo "$boot_dir/grub/fragments/$(basename "$i")" > "$boot_dir"/grub/fragment-index/$(printf "%06d" $n)
          cp -fL "$i/boot/for-bootloader/"/grub.part.cfg "$boot_dir/grub/fragments/$(basename "$i")"/ 2>/dev/null
          rm "$boot_dir/grub/fragments/$(basename "$i")" 2> /dev/null
          test -d "$boot_dir/grub/fragments/$(basename "$i")" || {
            mkdir "$boot_dir/grub/fragments/$(basename "$i")"
            cp -fL "$i/boot/for-bootloader/"/grub.part.cfg "$boot_dir/grub/fragments/$(basename "$i")"/ 2>/dev/null
            cp -L "$i/boot/for-bootloader/"/*.efi "$boot_dir"/kernels/
          }
        }
      done
      sync -f "$boot_dir/kernels"
      for i in "$boot_dir"/grub/fragments/*; do
        sed -re "s@^menuentry[^\"]*\"@&$(basename "$i") @"  "$i/grub.part.cfg" > "$i/grub.part.labeled.cfg"
        ( echo "$grubHeader"; cat "$i/grub.part.labeled.cfg" ) > "$i/grub.one.cfg"
      done
      cp "$boot_dir"/grub/grub.fragmented.cfg{,.old}
      sync -f "$boot_dir/grub"
      ( echo "$grubHeader"; cat "$boot_dir"/grub/fragment-index/* |
          sed -e 's@$@/grub.part.labeled.cfg@' | xargs cat ) > "$boot_dir"/grub/grub.fragmented.cfg
      cp -f "$boot_dir"/grub/grub{.fragmented,}.cfg
      sync -f "$boot_dir/grub"

