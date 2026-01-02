{ wrapFirefox, firefox-esr-unwrapped, firefox-esr, callPackage, fetchurl, runCommand }:
wrapFirefox 
(
  firefox-esr-unwrapped // { allowAddonSideload = true; requireSigning = false; }
)
{
  extraPrefsFiles = (firefox-esr-unwrapped.extraPrefsFiles or []) ++ [ ];
  extraPoliciesFiles = (firefox-esr-unwrapped.extraPoliciesFiles or []) ++ [ ];
  libName = "firefox";
  nativeMessagingHosts = [ 
    (callPackage ./extension.nix {}).host
  ];
  nixExtensions = [
    (callPackage ./extension.nix {}).ext
  ] ++ (callPackage ./upstream-extensions.nix {});
}

