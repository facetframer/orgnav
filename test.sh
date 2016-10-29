#!/bin/dash
set -o errexit
set -o nounset
set -o pipefail

here="$(dirname ${BASH_SOURCE[0]})"
cd $here;
elpa="$(mktemp -d)"

emacs -q --batch --eval "
(progn
(setq package-user-dir \"$elpa\")
(require 'package)
(add-to-list 'package-archives '(\"melpa-stable\" . \"http://stable.melpa.org/packages/\"))
(package-initialize 't)
(list-packages) ;; needed to update package list
(package-install-file \"bho.el\"))"
