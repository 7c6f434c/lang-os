set -x

readlink -f /dev/mapper/BuildBox--NVMe--Main-Swap > /sys/power/resume
swapon /dev/mapper/BuildBox--NVMe--Main-Swap

mount -t ext4 /dev/mapper/BuildBox--NVMe--Main-SystemRoot /new-root
mount -t btrfs /dev/mapper/BuildBox--NVMe--Main-Nix /new-root/nix
ls -ld /new-root/nix/store
chmod a-r /new-root/nix/store
ls -ld /new-root/nix/store
mount /dev/mapper/BuildBox--NVMe--Main-Home /new-root/home
mount /dev/mapper/BuildBox--NVMe--Main-Root /new-root/root
mount /dev/mapper/BuildBox--NVMe--Main-Var /new-root/var
mount /dev/mapper/BuildBox--NVMe--Main-VarDb /new-root/var/db
mount /dev/mapper/BuildBox--NVMe--Main-VarLog /new-root/var/log

mount /dev/mapper/BuildBox--HDD--Aux-ExperimentsData /new-root/media/ExperimentsData/

yes y | mkfs.ext4 /dev/mapper/BuildBox--NVMe--Main-Tmp

mount /dev/mapper/BuildBox--NVMe--Main-Tmp /new-root/tmp
chmod a+rwxt /new-root/tmp

mount -t vfat -L SECOND_EFI /new-root/boot

modprobe fuse

mount fuse -t fusectl /sys/fs/fuse/connections/
