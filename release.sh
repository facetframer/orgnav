#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

# Release a new version

emacs -Q -batch --eval '(progn (load-file "orgnav-version.el") (princ orgnav-version))'  2>/dev/null | {
    version="$(cat)"; git tag release-$version ; }
