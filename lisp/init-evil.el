;;; init-evil.el -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-funcs)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (evil-mode)

  :config
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    (setq-default evil-ex-search-persistent-highlight nil)



    (define-key evil-visual-state-map "p" 'evil-paste-after)
    (define-key evil-insert-state-map (kbd "C-r") 'evil-paste-from-register)
    (define-key evil-insert-state-map (kbd "C-;") 'flyspell-correct-previous)


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
    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)


    ))

(use-package evil-anzu
  :ensure t
  :after evil
  :demand t
  :init
  (global-anzu-mode t))

(use-package evil-collection
  :ensure t
  :demand t
  :config
  (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list))
  (evil-collection-init)

  (evil-define-key 'normal dired-mode-map
    (kbd "<RET>") 'dired-find-alternate-file
    (kbd "C-k") 'dired-up-directory
    "`" 'dired-open-term
    "z" 'dired-get-size
    ")" 'dired-omit-mode))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)
  (evil-set-undo-system 'undo-tree))


(use-package evil-leader
  :init
  (setq evil-leader/in-all-states t)
  (global-evil-leader-mode t)
  (setq evil-leader/non-normal-prefix "C-")
  (setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "gnus-.*-mode"))
  (evil-leader/set-leader "SPC")


  (evil-leader/set-key
    "<SPC>" 'execute-extended-command)

  :config
  (progn

    (evil-leader/set-key
      "f" 'my/file-command
      "b" 'my/buffer-command
      "u" 'universal-argument
      "p" 'my/project-command
      "0" 'select-window-0
      "1" 'select-window-1
      "2" 'select-window-2
      "3" 'select-window-3
      "w/" 'split-window-right
      "w-" 'split-window-below
      ":" 'execute-extended-command
      "'" 'vertico-repeat
      "t" 'my/toggle-command
      "wm" 'delete-other-windows
      "qq" 'save-buffers-kill-terminal
      "hh" 'zilongshanren/highlight-dwim
      "hc" 'zilongshanren/clearn-highlight
      "sj" 'my/imenu
      "en" 'flycheck-next-error
      "ry" 'consult-yank-pop
      "ep" 'flycheck-previous-error
      "el" 'flycheck-list-errors
      "x" 'switch-to-scratch-buffer
      "sp" 'consult-ripgrep
      "=" 'indent-buffer
      "oy" 'youdao-dictionary-search-at-point+
      "oo" 'zilongshanren/helm-hotspots
      "TAB" 'spacemacs/alternate-buffer
      "hdf" 'describe-function
      "v" #'er/expand-region
      "hdv" 'describe-variable
      "hdk" 'describe-key
      "gs" 'magit-status
      "gd" 'vc-diff
      "gg" 'xref-find-definitions
      "gr" 'xref-find-references)


    (use-package transient
      :ensure t
      :demand t)
    ;; file keymaps
    (transient-define-prefix my/file-command
      "Files"
      [["Find"
        ("f" "find-file" find-file)
        ("r" "find recent file" consult-recent-file)
        ("L" "locate file" consult-locate)
        ("d" "open directory" consult-dir)
        ("ed" "find emacs init file" open-my-init-file)]
       ["CRUD"
        ("s" "Save" save-buffer)
        ("S" "save all files" save-some-buffers)
        ("j" "Dired jump" dired-jump)
        ("y" "Copy Filename" copy-file-name)
        ("R" "Rename" my/rename-current-buffer-file)
        ("k" "Delete" my/delete-file-and-buffer)
        ("!" "Exec shell" my/exec-shell-on-buffer)
        ]])

    ;; buffer keymap
    (transient-define-prefix my/buffer-command
      "Buffer"
      [["Find"
        ("b" "switch buffer" switch-to-buffer)
        ("s" "switch to scratch buffer" create-scratch-buffer)
        ("i" "switch to ibuffer" ibuffer)
        ("f" "open current buffer directory" my-open-current-directory)]
       ["CRUD"
        ("k" "kill buffer" kill-buffer)
        ("K" "kill all other buffer" kill-other-buffers)
        ("r" "revert buffer" revert-buffer)
        ("d" "kill this buffer" kill-this-buffer)]])
    ;; workspace keymaps

    ;; window keymaps

    ;; toggle keymaps

    (transient-define-prefix my/toggle-command
      "Toggle"
      [["Tools"
        ("s" "syntax checher" flycheck-mode)
        ("S" "spell checher" flyspell-prog-mode)
        ("e" "corfu english helper" toggle-corfu-english-helper)
        ("n" "line number" my-toggle-line-numbber)
        ("w" "writing" distraction-free)
        ]])

    ;; git keymaps stduent

    ;; search keymaps

    ;; project keymaps
    (transient-define-prefix my/project-command
      "Project"
      [["Find"
        ("f" "File" project-find-file)
        ("r" "Recentf" consult-recent-file)
        ("s" "Search" project-find-regexp)
        ("d" "Dired" project-dired)
        ("b" "buffer" consult-project-buffer)]
       ["Progn"
        ("e" "Eshell" project-eshell)
        ("m" "Makefile" my/project-run-makefile-target)
        ("c" "Compile" project-compile)
        ("t" "ciTre" my/project-citre)
        ]
       ["Manage"
        ("p" "Project" project-switch-project)
        ("i" "Info" my/project-info)
        ("a" "Add" project-remember-projects-under)
        ("x" "Xelete" project-forget-project)]])

    ))

(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :init
  (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

  ;; (evilnc-default-hotkeys)
  )

(use-package evil-snipe
  :ensure t
  :init
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package bind-map
  :ensure t)

(use-package spaceleader
  :ensure nil
  :init
  (progn
    (require 'spaceleader)
    (leader-set-keys-for-major-mode 'org-mode
      "p" 'org-pomodoro
      "t" 'org-todo
      "e" 'org-set-effort
      ">" 'org-metaright
      "<" 'org-metaleft
      "J" 'org-metadown
      "K" 'org-metaup
      "T" 'org-set-tags-command
      "I" 'org-clock-in
      "O" 'org-clock-out
      "P" 'org-set-property
      "s" 'org-schedule)))




(provide 'init-evil)
