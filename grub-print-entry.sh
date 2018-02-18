#! /bin/sh

name="$1"
linux="$2"
initrd="$3"

shift ; shift ; shift

echo "
menuentry \"$name\" {
  linux  (\$drive1)/kernels/$(basename $linux) $*
  initrd (\$drive1)/kernels/$(basename $initrd)
}
"
