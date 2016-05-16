; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;;(when (>= emacs-major-version 24)
  ;;(setq package-archives '(("melpa" . "http://elpa.zilongshanren.com/melpa/")))
  ;;)

;;(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; add more personal func
;; new init-func.el 
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)


;; (require 'init-func.el)
;;(require 'init-packages)
;;(require 'init-ui)
;;(require 'init-better-defaults)
;;(require 'init-org)
;;(require 'init-keybindings)

;;(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))

;;(load-file custom-file)
















(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (package-build shut-up epl git commander f dash s))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
