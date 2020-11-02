#! /usr/bin/env bash

set -x

target="$HOME/mnt/query-fs/backing-store/saved-web/$(date +%Y/%m/%d/%H-%M-%S/)"
mkdir -p "$target"
target="$(mktemp -d -p "$target")"
basename="$( ( echo "$1"; date +%Y%m%d-%H%M%S; ) | sha256sum | sed -e 's/ .*//' )"

cd "$(dirname "$0")"

formparam="$1"

./thoughtful-theridion.bin --non-interactive --eval "(thoughtful-theridion::save-web-form (quote $formparam) \"$target\" \"$basename\" $2)"

shift; shift

if test -n "$1"; then 
        "$@" "$target/$basename.html.txt"
else
        echo "$target/$basename.html"
fi
