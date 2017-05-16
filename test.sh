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
cask build 2> >(grep -v -e "Warning: Unused lexical variable .org-capture-templates." -e "^Compiling" -e "^In toplevel form:$" >&2 )
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
(setq files (directory-files \".\" nil \"\\\\.el$\"))
(setq files (remove \"orgnav-pkg.el\" files))
(cl-loop for file in  files do (byte-compile-file file ))
(cl-loop for file in  files do (checkdoc-file file))
)" 2>&1  | grep -v -e "capture-function.*should appear in quotes" \
    -e "Warning: Unused lexical variable .org-capture-templates." | grep -v -F \
    -e "Warning (emacs):" \
    -e "Some lines are over 80 columns wide" \
    -e 'In toplevel form:' | grep -v '^$'


# Having problems getting this to
#  return 1 iff there are error lines, give up
#  error in way that doesn't hide errors
exit 1