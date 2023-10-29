#!/bin/sh -e
echo "Attempting startup..."
mkdir -p $HOME/.emacs.d/elpa/gnupg && gpg --homedir $HOME/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40;
emacs -q --batch \
        --eval "(message \"Testing...\")" \
        --eval "(let ((early-init-file (locate-user-emacs-file \"early-init.el\"))
                (user-init-file (locate-user-emacs-file \"init.el\")))
            (and (>= emacs-major-version 27) (load early-init-file))
            (load user-init-file))" \
                --eval "(message \"Testing...done\")"
echo "Startup successful"
