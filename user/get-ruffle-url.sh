#! /bin/sh
curl https://addons.mozilla.org/en-US/firefox/addon/ruffle_rs/ | tr '"' '\n' | grep '^https://.*[.]xpi' | head -n 1 
