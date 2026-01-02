{ wrapFirefox, firefox-devedition-unwrapped, firefox-devedition, callPackage, fetchurl, runCommand }:
wrapFirefox 
(
  firefox-devedition-unwrapped // { allowAddonSideload = true; }
)
{
  extraPrefsFiles = (firefox-devedition-unwrapped.extraPrefsFiles or []) ++ [ ];
  extraPoliciesFiles = (firefox-devedition-unwrapped.extraPoliciesFiles or []) ++ [ ];
  libName = "firefox-devedition";
  nativeMessagingHosts = [ 
    (callPackage ./extension.nix {}).host
  ];
  nixExtensions = [
    (callPackage ./extension.nix {}).ext
  ] ++ (callPackage ./upstream-extensions.nix {});
}

