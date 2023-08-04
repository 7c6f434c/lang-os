set -x

cryptsetup open --allow-discards /dev/disk/by-partlabel/ThinkPad-Main-Crypt ThinkPad-Main-CryptVolume

    udevadm trigger --action=add
    udevadm settle

vgchange -ay

    udevadm trigger --action=add
    udevadm settle

readlink -f /dev/mapper/ThinkPad--MainCrypt-Swap > /sys/power/resume
swapon /dev/mapper/ThinkPad--MainCrypt-Swap

mount -t ext4 /dev/mapper/ThinkPad--MainCrypt-SystemRoot /new-root
mount -t btrfs /dev/mapper/ThinkPad--MainCrypt-Nix /new-root/nix
ls -ld /new-root/nix/store
chmod a-r /new-root/nix/store
ls -ld /new-root/nix/store
mount /dev/mapper/ThinkPad--MainCrypt-Home /new-root/home
mount /dev/mapper/ThinkPad--MainCrypt-Root /new-root/root
mount /dev/mapper/ThinkPad--MainCrypt-Var /new-root/var
mount /dev/mapper/ThinkPad--MainCrypt-VarDb /new-root/var/db
mount /dev/mapper/ThinkPad--MainCrypt-VarLog /new-root/var/log

yes y | mkfs.ext4 /dev/mapper/ThinkPad--MainCrypt-Tmp

mount /dev/mapper/ThinkPad--MainCrypt-Tmp /new-root/tmp
chmod a+rwxt /new-root/tmp

mount -t vfat -L TP_SSD_EFI /new-root/boot

modprobe fuse

mount fuse -t fusectl /sys/fs/fuse/connections/
