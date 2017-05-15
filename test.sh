#!/bin/bash

# It seems like testing something as interactive as helm is difficult
#   so I'm not even going to try.

# However, I will *lint* the file, and make sure installation works

set -o errexit
set -o nounset
set -o pipefail

here="$(dirname ${BASH_SOURCE[0]})"
cd $here;

cask install
cask build
cask package

cask eval "
(progn
(require 'orgnav)
(require 'orgnav-tree)
(require 'orgnav-capture)
(require 'orgnav-clock)
(require 'orgnav)
(require 'orgnav-refile)
(require 'orgnav-tree)
(setq byte-compile-error-on-warn 't)
(byte-compile-file \"orgnav.el\")
(byte-compile-file \"orgnav-tree.el\")
(byte-compile-file \"orgnav-capture.el\")
(byte-compile-file \"orgnav-clock.el\")
(byte-compile-file \"orgnav-refile.el\")
(checkdoc-file \"orgnav.el\")
(checkdoc-file \"orgnav-tree.el\")
(checkdoc-file \"orgnav-capture.el\")
(checkdoc-file \"orgnav-clock.el\")
(checkdoc-file \"orgnav-refile.el\")
)" 2>&1  |
    grep -v "capture-function.*should appear in quotes" |
    grep -v -F \
         -e "Warning (emacs):" \
         -e "Some lines are over 80 columns wide"

if [ "$?" == 0 ]; then
   exit 1;
fi;

