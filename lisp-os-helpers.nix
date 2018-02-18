{
  pkgs, src, deps
}:
pkgs.lispPackages.buildLispPackage {
    inherit (pkgs) stdenv;
    inherit (pkgs.lispPackages) clwrapper;

    inherit src deps;

    baseName = "lisp-os-helpers";
    buildSystems = ["lisp-os-helpers"];
    description = "Local library for defining and interacting with Common Lisp system daemon";

    overrides = x: {
      postInstall = ''
        NIX_LISP_PRELAUNCH_HOOK='nix_lisp_run_single_form "(asdf:perform (quote asdf:monolithic-compile-bundle-op) :lisp-os-helpers)"' "$out"/bin/*-lisp-launcher.sh ""
        NIX_LISP_EARLY_OPTIONS="--non-interactive"
        NIX_LISP_PRELAUNCH_HOOK="nix_lisp_build_system \
          lisp-os-helpers '(function lisp-os-helpers/read-eval-print-once:read-eval-print)'" \
          "$out"/bin/*-lisp-launcher.sh
        mv "$out/lib/common-lisp/lisp-os-helpers/lisp-os-helpers" "$out/bin/lisp-os-helpers-eval-form"
      '';
    };
  }

