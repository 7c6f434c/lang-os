rec {
  pkgs = import <nixpkgs> {};
  firefoxProfile = import ../firefox-profile.nix {
    inherit pkgs;
    finalContent = "" + ./firefox-profile-skel;
  };
  firefoxLauncherSet = import ../wrapped-firefox-launcher.nix {
    inherit pkgs;
    baseProfile = firefoxProfile;
  };
  inherit (firefoxLauncherSet) firefoxLauncher;
}
