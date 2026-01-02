#! /bin/sh

curl https://addons.mozilla.org/en-US/firefox/addon/ublock-origin/ | tr '"' '\n' | grep '^https://.*[.]xpi' | head -n 1 
