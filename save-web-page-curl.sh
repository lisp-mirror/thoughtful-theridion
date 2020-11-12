#! /usr/bin/env bash

set -x

target="$HOME/mnt/query-fs/backing-store/saved-web/$(date +%Y/%m/%d/%H-%M-%S/)"
mkdir -p "$target"
target="$(mktemp -d -p "$target")"
basename="$( ( echo "$1"; date +%Y%m%d-%H%M%S; ) | sha256sum | sed -e 's/ .*//' )"

cd "$(dirname "$0")"

url="$1"
if ! echo "$url" | grep -E '^[a-z]+:' > /dev/null; then
        url="https://$url"
fi

echo "$url" > "$target/$basename.url"
curl --user-agent 'thoughtful theridion' -L "$url" > "$target/$basename.html"
./dump-html.sh "$url" < "$target/$basename.html" > "$target/$basename.html.txt"

shift; shift

if test -n "$1"; then 
        "$@" "$target/$basename.html.txt"
else
        echo "$target/$basename.html"
fi
