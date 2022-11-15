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

url="${url//\"/%22}"
url="${url//\\/%5c}"

./thoughtful-theridion.bin --non-interactive --eval "(thoughtful-theridion::save-web-page \"$url\" \"$target\" \"$basename\" $2 :referrer \"${WEB_PAGE_REFERRER//"/\\"}\")" >&2

shift; shift

if test -n "$1"; then 
        "$@" "$target/$basename.html.txt"
else
        echo "$target/$basename.html"
fi
