{ 
  pkgs ? import <nixpkgs> {}, firefox ? p: p.firefox
  , initialContent ? null, finalContent ? null
  , environment ? {}
}:
let
  _firefox = if builtins.isFunction firefox then (firefox pkgs) else firefox;
in
pkgs.runCommand "firefox-initialised-profile" environment ''
  mkdir -p "$out"
  echo "<script>window.close();</script>" > close.html
  echo '
        user_pref("app.normandy.first_run", false);
        user_pref("dom.allow_scripts_to_close_windows", true);
        user_pref("browser.shell.checkDefaultBrowser", false);
        user_pref("toolkit.telemetry.reportingpolicy.firstRun", false);
        user_pref("startup.homepage_welcome_url", "javascript:window.close()");
        user_pref("startup.homepage_welcome_url_additional", "");
        user_pref("datareporting.policy.firstRunURL", "javascript:window.close()");
        user_pref("browser.tabs.closeWindowWithLastTab", true);
        user_pref("browser.tabs.warnOnClose", false);
        user_pref("browser.tabs.warnOnCloseOtherTabs", false);
        user_pref("datareporting.healthreport.service.firstRun", false);
        user_pref("datareporting.policy.firstRunURL", "javascript:window.close();");
        user_pref("browser.shell.didSkipDefaultBrowserCheckOnFirstRun", true);
        user_pref("datareporting.policy.dataSubmissionPolicyAcceptedVersion", 9999);
       ' > "prefs.js"
  
  DISPLAY=:7 \
  "${pkgs.lib.getBin pkgs.utillinux}/bin/unshare" -U -r -m \
  sh -c '
   id
   set -x

   mkdir new-etc
   ${pkgs.lib.getBin pkgs.utillinux}/bin/mount --bind new-etc /etc
   echo 00000000000000000000000000000000 > /etc/machine-id
   mkdir /etc/fonts
   ln -s "${pkgs.makeFontsConf {fontDirectories = [];}}" /etc/fonts/fonts.conf

   "${pkgs.lib.getBin pkgs.xdummy}/bin/xdummy" :7 &
   while ! DISPLAY=:7 "${pkgs.lib.getBin pkgs.xorg.xprop}/bin/xprop" -root &> /dev/null; do
     sleep 1;
   done

   export HOME="$PWD"

   echo "$out"
   echo "$HOME"
   echo "$DISPLAY"
   
   "${pkgs.lib.getBin pkgs.icewm}/bin/icewm" &
   "${pkgs.lib.getBin pkgs.rxvt_unicode}/bin/urxvt" &
   "${pkgs.lib.getBin _firefox}/bin/firefox" -CreateProfile only
   cp -rf ~/.mozilla/firefox/*/* "$out"
   ls "$out"

   if test -n "${builtins.toString initialContent}"; then
     cp -rf "${builtins.toString initialContent}"/* "$out"
   fi
   chmod u+rw -R "$out"
   cp -fT prefs.js "$out/prefs.js"

   "${pkgs.lib.getBin _firefox}/bin/firefox" --profile "$out" --new-instance "file:///$PWD/close.html"
   echo "Firefox empty-run finished"
   ${pkgs.lib.getBin pkgs.procps}/bin/pkill xdummy

   echo "Exiting the namespaces"

   ${pkgs.lib.getBin pkgs.procps}/bin/pkill -9 -P $$
   echo "Killed child processes"
   ${pkgs.lib.getBin pkgs.procps}/bin/pkill -9 --nslist mnt --ns $$
   echo "Killed namespace processes"

   exit 0
  ' || true
  
  echo "Exited the namespaces"

  if test -n "${builtins.toString finalContent}"; then
    echo "${builtins.toString finalContent}";
    ls "${builtins.toString finalContent}";
    cp -rfT "${builtins.toString finalContent}" "$out"
  fi
  if test -f "$out/search.json"; then
    ${pkgs.lib.getBin pkgs.mozlz4a}/bin/mozlz4a "$out/search.json" "$out/search.json.mozlz4"
  fi

  echo "Done the post-copying"
  
  ${pkgs.lib.getBin pkgs.procps}/bin/pkill -9 -P $$ || echo "Hm, no child processes?"
  echo "Killed child processes"

  echo "Done"

  exit 0
''
