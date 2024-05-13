{
  pkgs, src, deps
}:
pkgs.sbcl.buildASDFSystem {
    pname = "lisp-os-helpers";
    version = "0.0-unstable";

    inherit src;

    lispLibs = deps;

    description = "Local library for defining and interacting with Common Lisp system daemon";

  }

