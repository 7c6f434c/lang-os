{ 
  pkgs ? import <nixpkgs> {}
  , browserPackage ? p: (p.callPackage ./user/ffde-with-policies.nix {})
  , browserName ? "firefox-devedition"
  , profileContent ? null
  , baseProfile ? import ./firefox-profile.nix { 
    inherit pkgs; 
    firefox=browserPackage; 
    firefoxName = browserName; 
    finalContent = profileContent; 
  }
  , name ? "firefox-launcher"
}:
rec {
  socatCmd = (pkgs.lib.getBin pkgs.socat) + "/bin/socat";
  firefoxCmd = (pkgs.lib.getBin (browserPackage pkgs)) + "/bin/" + browserName;
  unionfsCmd = (pkgs.lib.getBin pkgs.unionfs-fuse) + "/bin/unionfs";
  xdummyCmd = (pkgs.lib.getBin pkgs.xdummy) + "/bin/xdummy";
  xpropCmd = (pkgs.lib.getBin pkgs.xorg.xprop) + "/bin/xprop";
  fuserCmd = (pkgs.lib.getBin pkgs.psmisc) + "/bin/fuser";
  combineProfileScript = ''
    if test -n "$FIREFOX_PROFILE"; then
      _FIREFOX_PROFILE="$FIREFOX_PROFILE"
    else
      mkdir -p "/''${TMPDIR:-tmp}/ff.$USER/profiles/"
      _FIREFOX_PROFILE="$(mktemp -d -p "/''${TMPDIR:-/tmp}/ff.$USER/profiles/")"
    fi
    chmod og-rwx "$_FIREFOX_PROFILE"
    test -n "${baseProfile}" && yes n | cp -riT "${baseProfile}" "$_FIREFOX_PROFILE"
  '';
  homeScript = ''
    if test -z "$HOME" || ! test -d "$HOME"; then
      export HOME="$(mktemp -d)"
      _HOME_KILL="$HOME"
    else
      _HOME_KILL=
    fi;
  '';
  cleanupScript = ''
    if test -n "$_HOME_KILL"; then
      echo "Removing [[$_HOME_KILL]]" >&2
      rm -rf "$_HOME_KILL"
      echo "Removed [[$_HOME_KILL]]" >&2
    fi
    echo "making accessible [[$FIREFOX_PROFILE]]" >&2
    chmod a+rwX -R "$FIREFOX_PROFILE"/* 2> /dev/null
    test -n "$FIREFOX_PROFILE_KILL" &&
      rm -rf "$FIREFOX_PROFILE"
  '';
  firefoxProfileCombiner = pkgs.writeScriptBin "combine-firefox-profile" ''
    export FIREFOX_PROFILE="$1"
    ${combineProfileScript}
    echo "$_FIREFOX_PROFILE"
  '';
  displayScript=''
    if test -n "$FIREFOX_DISPLAY"; then
      export DISPLAY="$FIREFOX_DISPLAY"
      "${xdummyCmd}" "$DISPLAY" &
      while ! "${xpropCmd}" -root; do
        sleep 1;
      done
      echo "Virtual local DISPLAY=$DISPLAY" >&2
    fi
  '';
  firefoxLauncher = pkgs.writeScriptBin name ''#! /bin/sh
    ${homeScript}
    ${displayScript}
    echo "$FIREFOX_EXTRA_PREFS" >> "$FIREFOX_PROFILE/prefs.js"
    if test -z "$BROWSER_CONTROL_SOCKET" && test -n "$MARIONETTE_SOCKET"; then
      export BROWSER_CONTROL_SOCKET="$MARIONETTE_SOCKET"
    fi
    "${firefoxCmd}" --profile "$FIREFOX_PROFILE" --new-instance "$@"
    echo "${browserName} finished" >&2
    exit_value="$?"
    ${cleanupScript}
    exit $exit_value
  '';
  firefoxScripts = pkgs.runCommand "firefox-scripts" {} ''
    mkdir -p "$out/bin"
    ln -s "${firefoxProfileCombiner}"/bin/* "$out/bin"
    ln -s "${firefoxLauncher}"/bin/* "$out/bin"
    mkdir -p "$out/lib"
    ln -s "${baseProfile}" "$out/lib/profile"
  '';
}
