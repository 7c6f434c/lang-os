#! /bin/sh

socat -t 60 stdio /run/user/$UID/user-lisp-evaluator/socket; 
