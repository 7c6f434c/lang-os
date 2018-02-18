#! /bin/sh
export NIX_LISP_ASDF_LOAD='(require :asdf)'
export NIX_LISP_COMMAND=sbcl
export LISP_OS_HELPERS_PACKAGE="${LISP_OS_HELPERS_PACKAGE:-/var/current-system/sw}"
export USER_LISP_SHELL_RC="${USER_LISP_SHELL_RC:-$HOME/.user-lisp-shell}"

for i in "$LISP_OS_HELPERS_PACKAGE/lib/common-lisp-settings"/*-path-config.sh; do
	source "$i";
done

"$(dirname "$0")"/rlwrap-user-shell.sh common-lisp.sh --eval \
   '(defvar *lisp-os-helpers-package* 
          (uiop:getenv "LISP_OS_HELPERS_PACKAGE"))' \
   --load "$USER_LISP_SHELL_RC" "$@"
