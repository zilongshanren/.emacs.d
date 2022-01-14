;;; init-evil.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (evil-mode 1)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  :config
  (progn

    (dolist (mode '(ag-mode
                    flycheck-error-list-mode
                    occur-mode
                    occur-edit-mode
                    git-rebase-mode))
      (add-to-list 'evil-emacs-state-modes mode))

    (dolist (mode '(
                    occur-edit-mode))
      (add-to-list 'evil-normal-state-modes mode))


    (add-hook 'occur-mode-hook
              (lambda ()
                (evil-add-hjkl-bindings occur-mode-map 'emacs
                  (kbd "/") 'evil-search-forward
                  (kbd "n") 'evil-search-next
                  (kbd "N") 'evil-search-previous
                  (kbd "C-d") 'evil-scroll-down
                  (kbd "C-u") 'evil-scroll-up)))

    (add-hook 'occur-edit-mode-hook
              (lambda ()
                (evil-add-hjkl-bindings occur-edit-mode-map 'normal
                  (kbd "/") 'evil-search-forward
                  (kbd "n") 'evil-search-next
                  (kbd "N") 'evil-search-previous
                  (kbd "C-d") 'evil-scroll-down
                  (kbd "C-u") 'evil-scroll-up)))))

(with-eval-after-load 'evil
  (general-add-hook 'after-init-hook
                    (lambda (&rest _)
                      (when-let ((messages-buffer (get-buffer "*Messages*")))
                        (with-current-buffer messages-buffer
                          (evil-normalize-keymaps))))
                    nil
                    nil
                    t))

(use-package evil-leader
  :init
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "SPC" 'execute-extended-command
    "ff" 'find-file
    "fr" 'consult-recent-file
    "fs" 'save-buffer
    "bb" 'switch-to-buffer
    "bk" 'kill-buffer
    "pf" 'consult-buffer
    "ps" 'consult-ripgrep
    "0" 'select-window-0
    "1" 'select-window-1
    "2" 'select-window-2
    "3" 'select-window-3
    "fj" 'dired-jump
    "w/" 'split-window-right
    "w-" 'split-window-below
    ":" 'execute-extended-command
    "'" 'vertico-repeat
    "wm" 'delete-other-windows
    "qq" 'save-buffers-kill-terminal
    "sj" 'imenu
    "bd" 'kill-this-buffer
    "ts" 'flycheck-mode
    "sp" 'consult-ripgrep
    "TAB" 'spacemacs/alternate-buffer
    "fed" 'open-my-init-file
    "hdf" 'describe-function
    "hdv" 'describe-variable
    "hdk" 'describe-key
    "pb" 'consult-buffer
    "gs" 'magit-status
    "gg" 'citre-jump
    "gr" 'citre-peek)
  )

(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :init
  (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

  (evilnc-default-hotkeys))

(provide 'init-evil)
