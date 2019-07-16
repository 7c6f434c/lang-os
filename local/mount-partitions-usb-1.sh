#! /bin/sh

mount -t ext4 /dev/disk/by-label/LangOS_Rescue /new-root
mount -t vfat /dev/disk/by-label/LOS_EFI /new-root/boot
