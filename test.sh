#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

here="$(dirname ${BASH_SOURCE[0]})"
cd $here;
elpa="$(mktemp -d)"

# It seems like testing something as interactive as helm is difficult
#   so I'm not even going to try.

# However, I will *lint* the file, and make sure installation works

emacs -q --batch --eval "
(progn
(setq package-user-dir \"$elpa\")
(require 'package)
(add-to-list 'package-archives '(\"melpa-stable\" . \"http://stable.melpa.org/packages/\") t)
(package-initialize)
(package-refresh-contents)
(package-install-file \"orgnav.el\")
(setq byte-compile-error-on-warn 't)
(byte-compile-file \"orgnav.el\"))"
