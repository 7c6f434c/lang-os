{
  pkgs ? import <nixpkgs> {}
}:
pkgs.lib.makeExtensible (self: with self; {
  withDBus = pkgs.writeScriptBin "with-dbus" ''
    mytmp="''${TMPDIR:-/tmp}/temporary-bus-$(id -u)/"
    mkdir -p "$mytmp"
    dbusdir="$(mktemp -d -p "$mytmp")"
    echo "$dbusdir" >&2
    cat "${pkgs.dbus}/share/dbus-1/system.conf" | grep -v '[<]user[>]messagebus' > "$dbusdir/system.conf"
    "${pkgs.dbus}"/bin/dbus-daemon --nopidfile --nofork --config-file "${pkgs.dbus}"/share/dbus-1/session.conf --address "unix:path=$dbusdir/session"  >&2 &
    "${pkgs.dbus}"/bin/dbus-daemon --nopidfile --nofork --config-file "$dbusdir/system.conf" --address "unix:path=$dbusdir/system" >&2 &
    export DBUS_SESSION_BUS_ADDRESS="unix:path=$dbusdir/session"
    export DBUS_SYSTEM_BUS_ADDRESS="unix:path=$dbusdir/system"

    "$@"
    code="$?"

    kill %1
    kill %2

    rm -rf "$dbusdir"

    exit "$code"
  '';

  withPulseaudio = pkgs.writeScriptBin "with-pulseaudio" ''
    test -n "$DBUS_SESSION_BUS_ADDRESS" || exec "${withDBus}/bin/with-dbus" "$0" "$@"

    mytmp="''${TMPDIR:-/tmp}/temporary-bus-$(id -u)/"
    padir="$(mktemp -d -p "$mytmp")"

    test -n "$HOME" || export HOME="$padir"

    cat "${pkgs.pulseaudioLight}"/etc/pulse/default.pa |
      grep -Ev 'load-module module-(console-kit|systemd-.*|esound-protocol-unix)' |
      sed -e 's/module-udev-detect/module-detect/' > "$padir/default.pa"

    mkdir -p ~/.config/pulse/

    "${pkgs.pulseaudioLight}"/bin/pulseaudio --exit-idle-time=999999 -F "$padir"/default.pa &
    export LD_LIBRARY_PATH="''${LD_LIBRARY_PATH}''${LD_LIBRARY_PATH:+:}${pkgs.pulseaudioLight}/lib"
    "$@"

    code=$?

    kill %1
    rm -rf "$padir"

    exit $code
  '';
})
