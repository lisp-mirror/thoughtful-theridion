#! /bin/sh
unset NIX_LISP NIX_LISP_ASDF NIX_LISP_ASDF_PATHS NIX_LISP_COMMAND NIX_LISP_EARLY_OPTIONS NIX_LISP_EXEC_CODE NIX_LISP_LD_LIBRARY_PATH NIX_LISP_LOAD_FILE ASDF_OUTPUT_TRANSLATIONS
unset $(set | grep '^_.*NIX_LISP*' | cut -d = -f 1)

NIX_LISP_ASDF_PATHS="$(realpath "$(dirname "$0")")
$PWD"

dependencies="drakma dexador puri quri cl_plus_ssl cl-html5-parser parenscript cl-ppcre cl-json css-selectors css-selectors-simple-tree babel cl-unicode $THOUGHTFUL_THERIDION_EXTRA_DEPENDENCIES"

test -n "$THOUGHTFUL_THERIDION_NIX_GC_PIN" && {
        mkdir -p "$(dirname "$THOUGHTFUL_THERIDION_NIX_GC_PIN")"
        nix-build -E "with import <nixpkgs> {}; with lispPackages; buildEnv { name = ''thoughtful-theridion-dependencies''; paths = [ $dependencies ];}" -o "$THOUGHTFUL_THERIDION_NIX_GC_PIN"
}

for i in $dependencies; do
        . "$(nix-build --no-out-link '<nixpkgs>' -A lispPackages.$i)"/lib/common-lisp-settings/*-path-config.sh
done

export ASDF_OUTPUT_TRANSLATIONS=/nix/store:/nix/store

if test -n "$NO_RLWRAP"; then WRAP=bash; else
        WRAP="$(nix-build --no-out-link '<nixpkgs>' -A rlwrap)"/bin/rlwrap;
fi

"$WRAP" "$(nix-build --no-out-link '<nixpkgs>' -A lispPackages.clwrapper)"/bin/cl-wrapper.sh "$(nix-build --no-out-link '<nixpkgs>' -A sbcl)"/bin/sbcl $SBCL_EARLY_OPTIONS --eval "(require :thoughtful-theridion)" "$@"
