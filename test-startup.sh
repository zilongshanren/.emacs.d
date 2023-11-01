#!/bin/sh -e
echo "Attempting startup..."
export HOME="/home/runner/work/.emacs.d";
${EMACS:=emacs} -nw --batch \
                --eval '(progn
                        (defvar url-show-status)
                        (let ((debug-on-error t)
                              (url-show-status nil)
                              (user-emacs-directory default-directory)
                              (user-init-file (expand-file-name "init.el")))
                           (load-file user-init-file)
                           (run-hooks (quote after-init-hook))))'
echo "Startup successful"
