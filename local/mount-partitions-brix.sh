readlink -f /dev/mapper/BrixVG-Swap > /sys/power/resume
swapon /dev/mapper/BrixVG-Swap

mount -t ext4 /dev/mapper/BrixVG-Root /new-root
mount -t btrfs /dev/mapper/BrixVG-Nix /new-root/nix

mount /new-root/nix/store /new-root/nix/store -o bind,ro
mount /new-root/nix/store -o remount,bind,ro

mount /dev/mapper/BrixVG-Home /new-root/home
mount /dev/mapper/BrixVG-Var /new-root/var

yes y | mkfs.ext4 /dev/mapper/BrixVG-Tmp

mount /dev/mapper/BrixVG-Tmp /new-root/tmp
chmod a+rwxt /new-root/tmp

mount -t vfat -L BRIXEFI /new-root/boot

mount fuse -t fusectl /sys/fs/fuse/connections/
