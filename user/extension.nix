{ runCommandCC, zip, asymptote, imagemagickBig, cjson, ghostscript }:
runCommandCC "socket-control-webextension" {
  nativeBuildInputs = [ 
    zip asymptote imagemagickBig ghostscript 
  ];
  buildInputs = [ cjson ];
  outputs = [ "ext" "host" "out" ];
  passthru = {
    extid = "socket-control@localhost.senebofo";
  };
} ''
  mkdir -p "$host/bin" "$host/libexec"
  gcc \
    "${./control-extension/socket-listener.c}" \
    -o "$host/libexec/socket-listener.bin" \
    -lcjson \
  ;
  cp "${./control-extension/socket-listener.sh}" "$host/bin/socket-listener"
  chmod a+rx "$host/bin/socket-listener"

  mkdir -p "$host/lib/mozilla/native-messaging-hosts/"
  sed -e 's!@@@!${placeholder "host"}!' \
     < "${./control-extension/socket_listener.json}" \
     > "$host/lib/mozilla/native-messaging-hosts/socket_listener.json" \
  ;

  mkdir -p "$ext"

  mkdir ext
  cp "${./control-extension}"/{*.js,manifest.json} ext
  mkdir ext/icons
  asy -v -f pdf "${./control-extension/icons/icon.asy}" -o icon
  for s in 16 48 128; do
    magick -density 600 icon.pdf -resize "$s"x"$s" ext/icons/"$s".png
  done
  (
    cd ext
    zip "$ext/socket-control@localhost.senebofo.xpi" -r .
  )

  mkdir -p "$out/lib/src/webextension/socket-control@localhost.senebofo"
  cp -r ext/* \
    "$out/lib/src/webextension/socket-control@localhost.senebofo" \
  ;
  cp "${./control-extension/icons/icon.asy}" \
    "$out/lib/src/webextension/socket-control@localhost.senebofo/icons" \
  ;

''
