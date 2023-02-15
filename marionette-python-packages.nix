{ pkgs ? import <nixpkgs> {}}: 

# Warning: I do not care if all that stuff really works!
# Marionette doesn't really need most of the functionality.

pkgs.lib.makeExtensible (self: with self; {
  #callPackage = pkgs.pypy27.pkgs.newScope self;
  #callPackage = pkgs.pythonInterpreters.pypy27_prebuilt.pkgs.newScope self;
  callPackage = pkgs.newScope self;
  
  inherit (pkgs.pypy27.pkgs) 
    python
    buildPythonPackage 
    pytestCheckHook pipInstallHook setuptoolsBuildHook
    pythonOlder pythonAtLeast
    bootstrapped-pip
    ;
  isPy3k = false;
  isPyPy = true;
  isPy27 = true;
  isPy35 = false;
  isPy36 = false;
  isPy37 = false;
  isPy38 = false;
  isPy39 = false;

  ApplicationServices = null;
  CoreServices = null;

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

  apipkg = callPackage ./marionette-harness/apipkg.nix {};
  execnet = callPackage ./marionette-harness/execnet.nix {};

  pretend = callPackage ./marionette-harness/pretend.nix {};
  ipaddress = callPackage ./marionette-harness/ipaddress.nix {};

  psutil = callPackage ./marionette-harness/psutil.nix {};
  idna = callPackage ./marionette-harness/idna.nix {};
  pytest = callPackage ./marionette-harness/pytest.nix {};
  cryptography_vectors = callPackage ./marionette-harness/cryptography-vectors.nix {};
  pytest-runner = callPackage ./marionette-harness/pytest-runner.nix {};

  setuptools-scm = callPackage ./marionette-harness/setuptools-scm.nix {};
  setuptools = callPackage ./marionette-harness/setuptools.nix {};
  cffi = callPackage ./marionette-harness/cffi.nix {};
  pycparser = callPackage ./marionette-harness/pycparser.nix {};
  enum34 = callPackage ./marionette-harness/enum34.nix {};
  packaging = callPackage ./marionette-harness/packaging.nix {};
  pytz = callPackage ./marionette-harness/pytz.nix {};
  six = callPackage ./marionette-harness/six.nix {};
  attrs = callPackage ./marionette-harness/attrs.nix {};
  coverage = callPackage ./marionette-harness/coverage.nix {};
  mock = callPackage ./marionette-harness/mock.nix {};
  flaky = callPackage ./marionette-harness/flaky.nix {};
  nose = callPackage ./marionette-harness/nose.nix {};
  pexpect = callPackage ./marionette-harness/pexpect.nix {};
  sortedcontainers = callPackage ./marionette-harness/sortedcontainers.nix {};
  funcsigs = callPackage ./marionette-harness/funcsigs.nix {};
  unittest2 = callPackage ./marionette-harness/unittest2.nix {};
  pbr = callPackage ./marionette-harness/pbr.nix {};
  blessings = callPackage ./marionette-harness/blessings.nix {};
  pyparsing = callPackage ./marionette-harness/pyparsing.nix {};
  ptyprocess = callPackage ./marionette-harness/ptyprocess.nix {};
  pyasn1 = callPackage ./marionette-harness/pyasn1.nix {};
  atomicwrites = callPackage ./marionette-harness/atomicwrites.nix {};
  more-itertools = callPackage ./marionette-harness/more-itertools.nix {};
  pathlib2 = callPackage ./marionette-harness/pathlib2.nix {};
  pluggy = callPackage ./marionette-harness/pluggy.nix {};
  importlib-metadata = callPackage ./marionette-harness/importlib-metadata.nix {};
  configparser = callPackage ./marionette-harness/configparser.nix {};
  contextlib2 = callPackage ./marionette-harness/contextlibe2.nix {};
  zipp = callPackage ./marionette-harness/zipp.nix {};
  py = callPackage ./marionette-harness/py.nix {};
  wcwidth = callPackage ./marionette-harness/wcwidth.nix {};
  filelock = callPackage ./marionette-harness/filelock.nix {};
  toml = callPackage ./marionette-harness/toml.nix {};
  traceback2 = callPackage ./marionette-harness/traceback2.nix {};
  linecache2 = callPackage ./marionette-harness/linecache2.nix {};
  python-dateutil = callPackage ./marionette-harness/dateutil.nix {};
  pysocks = callPackage ./marionette-harness/pysocks.nix {};
  pytest-freezegun = callPackage ./marionette-harness/pytest-freezegun.nix {};
  freezegun = callPackage ./marionette-harness/freezegun.nix {};
  pytest-timeout = callPackage ./marionette-harness/pytest-timeout.nix {};
  pytest-cov = callPackage ./marionette-harness/pytest-cov.nix {};
  tornado = callPackage ./marionette-harness/tornado.nix {};
  trustme = callPackage ./marionette-harness/trustme.nix {};
  service-identity = callPackage ./marionette-harness/service-identity.nix {};
  pyasn1-modules = callPackage ./marionette-harness/pyasn1-modules.nix {};
  backports_functools_lru_cache = callPackage ./marionette-harness/backports_functools_lru_cache.nix {};
  pytest-black = callPackage ./marionette-harness/pytest-black.nix {};
  black = null;
  pytest-flake8 = null;
  aiohttp = null;
  aiodns = null;
  pycares = callPackage ./marionette-harness/pycares.nix {};
  typing = callPackage ./marionette-harness/typing.nix {};
  aiosignal = null;
  async-timeout = null;
  async_generator = null;
  asynctest = callPackage null;
  cchardet = callPackage ./marionette-harness/cchardet.nix {};
  charset-normalizer = callPackage ./marionette-harness/charset-normalizer.nix {};
  frozenlist = null;
  gunicorn = callPackage ./marionette-harness/gunicorn.nix {};
  idna-ssl = callPackage ./marionette-harness/idna-ssl.nix {};
  multidict = null;
  pytest-mock = callPackage ./marionette-harness/pytest-mock.nix {};
  re-assert = callPackage ./marionette-harness/re-assert.nix {};
  typing-extensions = callPackage ./marionette-harness/typing-extensions.nix {};
  yarl = callPackage ./marionette-harness/yarl.nix {};
  pytest-asyncio = null;
  aiohttp-cors = null;
  colorama = callPackage ./marionette-harness/colorama.nix {};
  dataclasses = null;
  mypy-extensions = callPackage ./marionette-harness/mypy-extensions.nix {};
  parameterized = callPackage ./marionette-harness/parameterized.nix {};
  pathspec = callPackage ./marionette-harness/pathspec.nix {};
  platformdirs = null;
  regex = callPackage ./marionette-harness/regex.nix {};
  tomli = callPackage ./marionette-harness/tomli.nix {};
  typed-ast = null;
  uvloop = null;
  cython = callPackage ./marionette-harness/cython.nix {};
  numpy = callPackage ./marionette-harness/numpy.nix {};
  appdirs = callPackage ./marionette-harness/appdirs.nix {};
  flake8 = callPackage ./marionette-harness/flake8.nix {};
  functools32 = callPackage ./marionette-harness/functools32.nix {};
  mccabe = callPackage ./marionette-harness/mccabe.nix {};
  pycodestyle = callPackage ./marionette-harness/pycodestyle.nix {};
  pyflakes = callPackage ./marionette-harness/pyflakes.nix {};
  flit-core = callPackage ./marionette-harness/flit-core.nix {};
  flit = callPackage ./marionette-harness/flit.nix {};
  requests_download = callPackage ./marionette-harness/requests_download.nix {};
  responses = callPackage ./marionette-harness/responses.nix {};
  testpath = callPackage ./marionette-harness/testpath.nix {};
  zipfile36 = null;
  cookies = callPackage ./marionette-harness/cookies.nix {};
  pytest-localserver = callPackage ./marionette-harness/pytest-localserver.nix {};
  werkzeug = callPackage ./marionette-harness/werkzeug.nix {};
  pytest-xprocess = callPackage ./marionette-harness/pytest-xprocess.nix {};
  watchdog = callPackage ./marionette-harness/watchdog.nix {};
  pathtools = callPackage ./marionette-harness/pathtools.nix {};
  pyyaml = callPackage ./marionette-harness/pyyaml.nix {};
  itsdangerous = callPackage ./marionette-harness/itsdangerous.nix {};
  scandir = callPackage ./marionette-harness/scandir.nix {};

})
