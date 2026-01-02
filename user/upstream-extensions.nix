{ runCommand, fetchurl }:
[
    (runCommand "ruffle-rs-xpi" 
    {
      extid = "{b5501fd1-7084-45c5-9aa6-567c2fcf5dc6}";
    }
    ''
      mkdir -p "$out"
      cp -v "${(fetchurl {
        url = "https://addons.mozilla.org/firefox/downloads/file/4597555/ruffle_rs-0.2.0.25287.xpi";
        hash = "sha256-7fFT9vPLZroWdC8WftsqBKcRq9qfmXKao3GBv5878qc=";
      })}" "$out/$extid.xpi"
    '')
    (runCommand "ublock-origin-xpi" 
    {
      extid = "uBlock0@raymondhill.net";
    }
    ''
      mkdir -p "$out"
      cp -v "${(fetchurl {
        url = "https://addons.mozilla.org/firefox/downloads/file/4629131/ublock_origin-1.68.0.xpi";
        hash = "sha256-XK9KvaSUAYhBIioSFWkZu92MrYKng8OMNrIt1kJwQxU=";
      })}" "$out/$extid.xpi"
    '')
    # (runCommand "youtube-no-translation-xpi" 
    # {
    #   extid = "{9a3104a2-02c2-464c-b069-82344e5ed4ec}";
    # }
    # ''
    #   mkdir -p "$out"
    #   cp -v "${(fetchurl {
    #     url = "https://addons.mozilla.org/firefox/downloads/file/4622331/youtube_no_translation-2.18.2.xpi";
    #     hash = "sha256-jr94v26SnUqo0AjG6sawHKt38cBFSX45l1/pDoJDvE4=";
    #   })}" "$out/$extid.xpi"
    # '')
]
