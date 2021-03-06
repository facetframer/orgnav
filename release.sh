#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

# Release a new version
if ! git diff-index --quiet HEAD --; then
    echo >&2 "Checkout is not clean"
    git status
    exit 1
fi;

cat orgnav.el | grep ';; Version'  |  cut -d : -f 2 | sed -E 's/^ +//' | {
    version="$(cat)"; git tag release-$version;
    SSH_AUTH_SOCK= git push public release-$version:master
    SSH_AUTH_SOCK= git push public release-$version;
}
