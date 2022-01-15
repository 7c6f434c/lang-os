{ pkgs ? import <nixpkgs> {}}: 
pkgs.lib.makeExtensible (self: with self; {
  callPackage = pkgs.pypy2Packages.newScope self;
  manifestparser = callPackage ./marionette-harness/manifestparser.nix { };
  marionette_driver = callPackage ./marionette-harness/marionette_driver.nix { };
  marionette-harness = callPackage ./marionette-harness { };
  mozcrash = callPackage ./marionette-harness/mozcrash.nix { };
  mozdevice = callPackage ./marionette-harness/mozdevice.nix { };
  mozfile = callPackage ./marionette-harness/mozfile.nix { };
  mozhttpd = callPackage ./marionette-harness/mozhttpd.nix { };
  mozinfo = callPackage ./marionette-harness/mozinfo.nix { };
  mozlog = callPackage ./marionette-harness/mozlog.nix { };
  moznetwork = callPackage ./marionette-harness/moznetwork.nix { };
  mozprocess = callPackage ./marionette-harness/mozprocess.nix { };
  mozprofile = callPackage ./marionette-harness/mozprofile.nix { };
  mozrunner = callPackage ./marionette-harness/mozrunner.nix { };
  moztest = callPackage ./marionette-harness/moztest.nix { };
  mozversion = callPackage ./marionette-harness/mozversion.nix { };
  mozterm = callPackage ./marionette-harness/mozterm.nix {};

  hpack = callPackage ./marionette-harness/hpack.nix { };
  h2 = callPackage ./marionette-harness/h2.nix { };
  hyperframe = callPackage ./marionette-harness/hyperframe.nix { };
  wptserve = callPackage ./marionette-harness/wptserve.nix { };

  pytest-forked = callPackage ./marionette-harness/pytest-forked.nix {};
  requests = callPackage ./marionette-harness/requests.nix {};
  browsermob-proxy = callPackage ./marionette-harness/browsermob-proxy.nix {};

  certifi = callPackage ./marionette-harness/certifi.nix {};
  urllib3 = callPackage ./marionette-harness/urllib3.nix {};

  hypothesis = callPackage ./marionette-harness/hypothesis.nix {};
  pytest-xdist = callPackage ./marionette-harness/pytest-xdist.nix {};
  pytest_xdist = pytest-xdist;
  chardet = callPackage ./marionette-harness/chardet.nix {};
  cryptography = callPackage ./marionette-harness/cryptography.nix {};
  iso8601 = callPackage ./marionette-harness/iso8601.nix {};
  pyopenssl = callPackage ./marionette-harness/pyopenssl.nix {};
})
