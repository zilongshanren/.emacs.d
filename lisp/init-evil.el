;;; init-evil.el -*- lexical-binding: t no-byte-compile: t -*-


(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (evil-mode)

  (with-eval-after-load 'evil
    (general-add-hook 'after-init-hook
                      (lambda (&rest _)
                        (when-let ((messages-buffer (get-buffer "*Messages*")))
                          (with-current-buffer messages-buffer
                            (evil-normalize-keymaps)
                            (evil-leader-mode 1)
                            )))
                      nil
                      nil
                      t))
  :config
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    (setq-default evil-ex-search-persistent-highlight nil)

    ;; (adjust-major-mode-keymap-with-evil "git-timemachine")
    ;; (adjust-major-mode-keymap-with-evil "tabulated-list")

    (define-key evil-visual-state-map "p" 'evil-paste-after)
    (define-key evil-insert-state-map (kbd "C-r") 'evil-paste-from-register)
    (define-key evil-insert-state-map (kbd "C-;") 'flyspell-correct-previous)

    (evil-set-initial-state 'minibufffer-inactive-mode 'emacs)
    (evil-set-initial-state 'makey-key-mode 'emacs)
    (evil-set-initial-state 'prodigy-mode 'emacs)
    (evil-set-initial-state 'org-agenda-mode 'normal)

    ;;mimic "nzz" behaviou in vim
    (defadvice evil-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))


    (defun my-evil-yank ()
      (interactive)
      (save-excursion
        (call-interactively 'evil-yank))
      (backward-char))

    (define-key evil-visual-state-map (kbd "y") 'my-evil-yank)

    (define-key evil-normal-state-map
      (kbd "Y") 'zilongshanren/yank-to-end-of-line)

    (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))

    (define-key evil-normal-state-map (kbd "g[")
      (lambda () (interactive) (beginning-of-defun)))

    (define-key evil-normal-state-map (kbd "g]")
      (lambda () (interactive) (end-of-defun)))

    (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
    (define-key evil-motion-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-motion-state-map (kbd "] b") 'next-buffer)
    (define-key evil-normal-state-map (kbd "M-y") 'consult-yank-pop)

    (define-key evil-emacs-state-map (kbd "s-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "s-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key evil-emacs-state-map (kbd "s-b") 'backward-word)
    (define-key evil-insert-state-map (kbd "s-b") 'backward-word)



    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)

    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-visual-state-map (kbd "C-r") 'zilongshanren/evil-quick-replace)
    (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
    (define-key evil-visual-state-map (kbd "mp") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
    (define-key evil-visual-state-map (kbd "mf") 'mc/mark-all-like-this-in-defun)


    ;; in spacemacs, we always use evilify miscro state
    (evil-add-hjkl-bindings prodigy-mode-map 'emacs)
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
    ;; Don't move back the cursor one position when exiting insert mode
    (setq evil-move-cursor-back nil)

    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)

    (evil-define-key 'emacs term-raw-map (kbd "C-w") 'evil-delete-backward-word)


    (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
          evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
          evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
          evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
          evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
          evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))
    (setq evil-insert-state-cursor '("chartreuse3" bar))
    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state))

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
                (kbd "C-u") 'evil-scroll-up))))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  (evil-set-undo-system 'undo-tree))


(use-package evil-leader
  :init
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>")


  (evil-leader/set-key
    "<SPC>" 'execute-extended-command)

  (evil-leader/set-key
    "ff" 'find-file
    "fr" 'consult-recent-file
    "fs" 'save-buffer
    "bb" 'switch-to-buffer
    "bk" 'kill-buffer
    "pf" 'project-find-file
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
    "tn" 'my-toggle-line-numbber
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
