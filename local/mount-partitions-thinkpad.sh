set -x

readlink -f /dev/mapper/ThinkPadMain-Swap > /sys/power/resume
swapon /dev/mapper/ThinkPadMain-Swap

mount -t ext4 /dev/mapper/ThinkPadMain-SystemRoot /new-root
mount -t btrfs /dev/mapper/ThinkPadMain-Nix /new-root/nix
ls -ld /new-root/nix/store
chmod a-r /new-root/nix/store
ls -ld /new-root/nix/store
mount /dev/mapper/ThinkPadMain-Home /new-root/home
mount /dev/mapper/ThinkPadMain-Root /new-root/root
mount /dev/mapper/ThinkPadMain-Var /new-root/var
mount /dev/mapper/ThinkPadMain-VarDb /new-root/var/db
mount /dev/mapper/ThinkPadMain-VarLog /new-root/var/log

yes y | mkfs.ext4 /dev/mapper/ThinkPadMain-Tmp

mount /dev/mapper/ThinkPadMain-Tmp /new-root/tmp
chmod a+rwxt /new-root/tmp

mount -t vfat -L NIXOS_EFI /new-root/boot

modprobe fuse

mount fuse -t fusectl /sys/fs/fuse/connections/
