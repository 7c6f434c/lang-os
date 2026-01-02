#! /bin/sh
curl https://addons.mozilla.org/de/firefox/addon/youtube-no-translation/ | tr '"' '\n' | grep '^https://.*[.]xpi' | head -n 1 
