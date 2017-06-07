#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

here="$(dirname ${BASH_SOURCE[0]})"
cd "$here"

if ! git diff-index --quiet HEAD --; then
    echo >&2 "Checkout is not clean"
    git status 2>&1
    exit 1
fi;


# Bump version number
<orgnav.el  grep "Version" | cut -d: -f 2 | sed -E 's/^ +//'  | {
    IFS=. read  incompatible feature bug
    case "${1:-}" in
        bug)
            bug=$(($bug + 1))
            ;;
        feature|deprecation)
            feature=$(($feature +1))
            bug=0
            ;;
        incompatible)
            incompatible=$(($incompatible +1))
            bug=0
            feature=0
            ;;
        *)
            echo "usage: $0 bug|feature|deprecation|incompatible" 2>&1
            exit 1;
            ;;
    esac;

    new_version="$incompatible.$feature.$bug"
    echo "$new_version"
    sed "s/Version:.*/Version: $new_version/" -i orgnav.el

}
