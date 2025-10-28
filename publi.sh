#!/usr/bin/env bash

# Run Emacs Lisp publishing script
#
# Copied from : https://github.com/Panadestein/emacsd/blob/master/publi.sh
emacs -Q --script build-site.el
