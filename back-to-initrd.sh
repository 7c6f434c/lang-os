#!/bin/sh

trap : 15

export PATH=/run/current-system/sw/bin
cd /
mkdir /initrd
mount initramfs -t tmpfs initrd
cd initrd
gunzip < /run/current-system/boot/initrd | cpio -i
mkdir new-root tmp

test -e /run/post-backpivot-command && {
        cat /run/post-backpivot-command > post-backpivot-command
        chmod a+x post-backpivot-command
        rm /run/post-backpivot-command
}

echo "#!/bin/sh" >> post-pivot
cat << EOF >> post-pivot
  cp "/new-root/$1" .

  chvt 1
  mkdir /run

  mount --move /new-root/proc /proc
  mount --move /new-root/sys /sys
  mount --move /new-root/dev /dev
  mount --move /new-root/run /run

  for signal in 15 2 9 9; do
          cat /proc/mounts | cut -d ' ' -f 2 | grep /new-root | tac | xargs -n1 umount
          mountpoint /new-root || break
          cat /proc/mounts | cut -d ' ' -f 2 | grep /new-root | tac | xargs -n1 fuser -k -$signal
          cat /proc/mounts | cut -d ' ' -f 2 | grep /new-root | tac | xargs -n1 umount
          mountpoint /new-root || break
          sleep 0.3
  done

  mountpoint /new-root

  vgchange -an
  for i in /dev/mapper/*; do
          cryptsetup close $i
  done
  vgchange -an

  test -n "$1" && /bin/sh "$(basename "$1")" < /dev/tty1 &> /dev/tty1
  test -e ./post-backpivot-command && ./post-backpivot-command < /dev/tty1 &> /dev/tty1

  while true; do /bin/sh -i; done
EOF
chmod a+x post-pivot
export PATH=/run/current-system/sw/bin:/init-tools/bin:/busybox/bin

trap : 15 2 3 6

kill -15 -1
sleep 1
kill -2 -1
sleep 0.5
kill -3 -1
sleep 0.5
kill -6 -1
sleep 0.2
kill -9 -1
sleep 0.2

pivot_root . ./new-root
exec chroot . ./post-pivot

