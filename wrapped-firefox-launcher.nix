{ 
  pkgs ? import <nixpkgs> {}, firefox ? p: p.firefox, firefoxName ? "firefox"
  , marionette ? p: p.python2Packages.marionette-harness
  , profileContent ? null
  , baseProfile ? import ./firefox-profile.nix { inherit pkgs firefox; finalContent = profileContent; }
  , name ? "firefox-launcher"
}:
rec {
  socatCmd = (pkgs.lib.getBin pkgs.socat) + "/bin/socat";
  unshareCmd = (pkgs.lib.getBin pkgs.utillinux) + "/bin/unshare";
  firefoxCmd = (pkgs.lib.getBin pkgs.firefox) + "/bin/" + firefoxName;
  unionfsCmd = (pkgs.lib.getBin pkgs.unionfs-fuse) + "/bin/unionfs";
  xdummyCmd = (pkgs.lib.getBin pkgs.xdummy) + "/bin/xdummy";
  xpropCmd = (pkgs.lib.getBin pkgs.xorg.xprop) + "/bin/xprop";
  fuserCmd = (pkgs.lib.getBin pkgs.psmisc) + "/bin/fuser";
  marionette_ = if builtins.isFunction marionette then marionette pkgs else marionette;
  marionetteEnv = pkgs.runCommand "marionette-env" { buildInputs = [ marionette_ ]; } ''
    echo "export PYTHONPATH='$PYTHONPATH'; export PATH='$PATH'" > "$out"
  '';
  marionettePythonPrologue = ''
    from marionette_driver.marionette import Marionette;
    import os;
    import sys;
    session = Marionette(host="127.0.0.1",port=os.getenv("MARIONETTE_PORT") or 2828,bin=False,socket_timeout=3600,startup_timeout=1);
    session.start_session(timeout=1) or exit()
    while True:
      try:
        code = sys.stdin.readline()
        if not code:
          exit()
        value = (eval(code))
        if value != None:
          print(value)
      except EOFError:
        exit()
      except Exception as e:
        print(e)
  '';
  combineProfileScript = ''
    if test -n "$FIREFOX_PROFILE"; then
      _FIREFOX_PROFILE="$FIREFOX_PROFILE"
    else
      mkdir -p "/''${TMPDIR:-tmp}/ff.$USER/profiles/"
      _FIREFOX_PROFILE="$(mktemp -d -p "/''${TMPDIR:-/tmp}/ff.$USER/profiles/")"
    fi
    test -n "${baseProfile}" && yes n | cp -riT "${baseProfile}" "$_FIREFOX_PROFILE"
  '';
  homeScript = ''
    if test -z "$HOME"; then
      HOME="$(mktemp -d)"
      _HOME_KILL="$HOME"
    else
      _HOME_KILL=
    fi;
  '';
  marionettePythonRunner = pkgs.writeScript "marionette-runner" ''
    python -u -c '${marionettePythonPrologue}'
  '';
  marionetteScript = ''
    if test -n "$MARIONETTE_SOCKET"; then
      export MARIONETTE_PORT="''${MARIONETTE_PORT:-2828}"
      export PYTHONIOENCODING=utf-8:surrogateescape
      (
        source "${marionetteEnv}"
        "${socatCmd}" -t 60 unix-listen:"$MARIONETTE_SOCKET",forever,fork,rcvbuf=1,sndbuf=1 exec:'${marionettePythonRunner}',rcvbuf=1,sndbuf=1 &
        "${socatCmd}" -t 60 unix-listen:"$MARIONETTE_SOCKET.port",forever,fork tcp-connect:127.0.0.1:"$MARIONETTE_PORT" &
	while ! ( test -e "$MARIONETTE_SOCKET" && test -e "$MARIONETTE_SOCKET.port" ; ); do
	  sleep 0.1
	done
	chmod a+rw "$MARIONETTE_SOCKET"{,.port}
        echo "MARIONETTE_SOCKET=$MARIONETTE_SOCKET" >&2
      )
    fi
  '';
  cleanupScript = ''
    if test -n "$_HOME_KILL"; then
      echo "Removing [[$_HOME_KILL]]" >&2
      rm -rf "$_HOME_KILL"
      echo "Removed [[$_HOME_KILL]]" >&2
    fi
    if test -n "$MARIONETTE_SOCKET"; then
      "${fuserCmd}" "$MARIONETTE_SOCKET" -k -s 2> /dev/null
      rm -f "$MARIONETTE_SOCKET"
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
  firefoxLauncher = pkgs.writeScriptBin name ''
    ${homeScript}
    ${marionetteScript}
    ${displayScript}
    echo "$FIREFOX_EXTRA_PREFS" >> "$FIREFOX_PROFILE/prefs.js"
    if test -n "$MARIONETTE_PORT"; then
      sed -e '/marionette[.]defaultPrefs[.]port/d' -i "$FIREFOX_PROFILE/prefs.js"
      echo "user_pref(\"marionette.defaultPrefs.port\",\"$MARIONETTE_PORT\");" >> "$FIREFOX_PROFILE/prefs.js"
      "${firefoxCmd}" --profile "$FIREFOX_PROFILE" --new-instance --marionette "$@"
    else
      "${firefoxCmd}" --profile "$FIREFOX_PROFILE" --new-instance "$@"
    fi
    echo "${firefoxName} finished" >&2
    exit_value="$?"
    ${cleanupScript}
    exit $exit_value
  '';
  firefoxScripts = pkgs.runCommand "firefox-scripts" {} ''
    mkdir -p "$out/bin"
    ln -s "${firefoxProfileCombiner}"/bin/* "$out/bin"
    ln -s "${firefoxLauncher}"/bin/* "$out/bin"
  '';
}
