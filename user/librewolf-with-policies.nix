{ wrapFirefox, librewolf-unwrapped, firefox-unwrapped, librewolf, callPackage, fetchurl, runCommand }:
wrapFirefox 
(
  librewolf-unwrapped
)
{
  extraPrefsFiles = (librewolf-unwrapped.extraPrefsFiles) ++ [ ];
  extraPoliciesFiles = (librewolf-unwrapped.extraPoliciesFiles) ++ [ ];
  libName = "librewolf";
  nativeMessagingHosts = [ 
    (callPackage ./extension.nix {}).host
  ];
  nixExtensions = [
    (callPackage ./extension.nix {}).ext
  ] ++ (callPackage ./upstream-extensions.nix {});
}

