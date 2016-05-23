;; (package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)


(require 'pallet)
(pallet-mode t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; (require 'init-func.el)
(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-keybindings)

(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))

(pp (macroexpand '(use-package xxxx)))

(pp (macroexpand '(use-package xxxx
  :init
  (progn 
    (setq my-name "guanghui")
    (setq my-age 28)
    )
  :config
  (setq my-dog "peter")
  :defer t
  )))

(load-file custom-file)
