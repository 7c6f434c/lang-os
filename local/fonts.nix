{pkgs, ...}:
  {
    enableGhostscriptFonts = true;
	
	# So there is a big directory in current-system 
	# with all fonts.
    enableFontDir = true;
    									
    fonts = ([
      pkgs.dejavu_fonts
      (pkgs.dejavu_fonts + "/share/fonts/truetype/")
      pkgs.libertine
      pkgs.liberation_ttf
      (pkgs.unifont // {bdf = null;})
      pkgs.unifont_upper
      pkgs.freefont_ttf
      pkgs.clearlyU
      pkgs.ucs-fonts
      pkgs.wqy_zenhei
      pkgs.junicode
      pkgs.lmodern
      pkgs.arkpandora_ttf
      pkgs.andagii
      pkgs.anonymousPro
      pkgs.inconsolata
      pkgs.theano
      pkgs.oldstandard
      pkgs.tempora_lgc
      pkgs.gentium
      pkgs.cm_unicode
      pkgs.lmmath
      pkgs.comic-neue
      pkgs.iosevka-bin
      /*pkgs.symbola*/
      (pkgs.ghostscript + "/share/ghostscript/fonts/")
      pkgs.mph_2b_damase

      pkgs.noto-fonts
      pkgs.noto-fonts-lgc-plus
      pkgs.noto-fonts-cjk-sans
      pkgs.noto-fonts-cjk-serif
      pkgs.noto-fonts-color-emoji
      pkgs.noto-fonts-emoji-blob-bin
      pkgs.noto-fonts-monochrome-emoji

      pkgs.paratype-pt-mono
      pkgs.paratype-pt-sans
      pkgs.paratype-pt-serif

      pkgs.dejavu_fonts

      pkgs.emojione

      pkgs.fira
    ]);

    enableCoreFonts = false;
  }
