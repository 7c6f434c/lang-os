(import ./test-system.nix {}).extend ( self: super: {
  stage1 = super.stage1.extend (s1self: s1super: {
    mountScript = ''
      modprobe atkbd
      modprobe usbhid
      modprobe hid-generic
      modprobe mac-hid
      modprobe xhci-hcd
      modprobe ehci-hcd
      
      sh ${./mount-partitions-thinkpad.sh}
    '';
  });
})
