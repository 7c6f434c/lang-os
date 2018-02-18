#! /bin/sh

fsuuid="$(grub-probe "$1" -t fs_uuid)"
if test -z "$fsuuid"; then
        fsdev="$(cat /proc/mounts | cut -d ' ' -f 1,2 | grep " $1\$" | cut -d ' ' -f 1)"
        fsuuid="$(blkid -o value -s UUID "$fsdev")"
fi

echo "
# Automatically generated

search --set=drive1 --fs-uuid $fsuuid

if [ -s \$prefix/grubenv ]; then
  load_env
fi

insmod efi_gop
insmod efi_uga
insmod font

if loadfont (\$drive1)//grub/fonts/unicode.pf2; then
  insmod gfxterm
  set gfxmode=auto
  set gfxpayload=keep
  terminal_output gfxterm
fi

set default=0 
set timeout=15
";
