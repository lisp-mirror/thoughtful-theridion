#! /usr/bin/env bash
unset NIX_LISP NIX_LISP_ASDF NIX_LISP_ASDF_PATHS NIX_LISP_COMMAND NIX_LISP_EARLY_OPTIONS NIX_LISP_EXEC_CODE NIX_LISP_LD_LIBRARY_PATH NIX_LISP_LOAD_FILE ASDF_OUTPUT_TRANSLATIONS
unset $(set | grep '^_.*NIX_LISP*' | cut -d = -f 1)

export CL_SOURCE_REGISTRY="$(realpath "$(dirname "$0")"):$PWD${CL_SOURCE_REGISTRY:+:}$CL_SOURCE_REGISTRY"

dependencies="drakma dexador puri quri cl_plus_ssl cl-html5-parser parenscript cl-ppcre cl-json css-selectors css-selectors-simple-tree babel cl-unicode bordeaux-threads $THOUGHTFUL_THERIDION_EXTRA_DEPENDENCIES"

drv="$(nix-instantiate -E "with import <nixpkgs> {}; sbcl.withPackages (ps: with ps; [ $dependencies ] )" --add-root "$THOUGHTFUL_THERIDION_NIX_GC_PIN_DRV")"
test -n "$THOUGHTFUL_THERIDION_NIX_GC_PIN" && nix-store -r "$drv" -o "$THOUGHTFUL_THERIDION_NIX_GC_PIN"
path="$(nix-store -r "$drv")"

if test -n "$NO_RLWRAP"; then WRAP=env; else
        WRAP="$(nix-build --no-out-link '<nixpkgs>' -A rlwrap)"/bin/rlwrap;
fi

echo "Lisp package: $path" >&2

"$WRAP" "$path"/bin/sbcl $SBCL_EARLY_OPTIONS --eval "(require :asdf)" --eval "(require :thoughtful-theridion)" "$@"
