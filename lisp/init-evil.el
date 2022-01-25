;;; init-evil.el -*- lexical-binding: t no-byte-compile: t -*-


(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (evil-mode)


  :config
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    (setq-default evil-ex-search-persistent-highlight nil)

    (evil-define-key 'normal dired-mode-map
      (kbd "C-k") 'zilongshanren/dired-up-directory
      ;; "<RET>" 'dired-find-alternate-file
      "E" 'dired-toggle-read-only
      "C" 'dired-do-copy
      "<mouse-2>" 'my-dired-find-file
      "`" 'dired-open-term
      "gr" 'revert-buffer
      "z" 'dired-get-size
      "c" 'dired-copy-file-here
      "f" 'consult-buffer
      ")" 'dired-omit-mode
      "<" 'beginning-of-buffer
      ">" 'end-of-buffer)

    ;; (adjust-major-mode-keymap-with-evil "git-timemachine")
    ;; (adjust-major-mode-keymap-with-evil "tabulated-list")

    (define-key evil-visual-state-map "p" 'evil-paste-after)
    (define-key evil-insert-state-map (kbd "C-r") 'evil-paste-from-register)
    (define-key evil-insert-state-map (kbd "C-;") 'flyspell-correct-previous)

    ;; set evil init states
    (dolist (m '(minibuffer-inactive-mode
                 makey-key-mode
                 prodigy-mode
                 ag-mode
                 flycheck-error-list-mode
                 git-rebase-mode))
      (add-to-list 'evil-emacs-state-modes m))

    (dolist (m '(wdired-mode
                 occur-edit-mode
                 xref--xref-buffer-mode
                 ))
      (add-to-list 'evil-normal-state-modes m))

    (dolist (m '(eww-mode))
      (add-to-list 'evil-motion-state-modes m))


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
    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)



    (add-hook 'xref--xref-buffer-mode-hook
              (lambda ()
                (evil-add-hjkl-bindings xref--xref-buffer-mode-map 'normal
                  (kbd "RET") 'xref-goto-xref
                  (kbd "q") 'quit-window)))

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
      "wm" 'delete-other-windows
      "qq" 'save-buffers-kill-terminal
       "hh" 'zilongshanren/highlight-dwim
       "hc" 'zilongshanren/clearn-highlight
      "sj" 'imenu
      "ts" 'flycheck-mode
      "tn" 'my-toggle-line-numbber
      "en" 'flycheck-next-error
      "ep" 'flycheck-previous-error
      "el" 'flycheck-list-errors
      "sp" 'consult-ripgrep
      "=" 'indent-buffer
      "oy" 'youdao-dictionary-search-at-point+
      "TAB" 'spacemacs/alternate-buffer
      "hdf" 'describe-function
      "v" #'er/expand-region
      "hdv" 'describe-variable
      "hdk" 'describe-key
      "gs" 'magit-status
      "gd" 'vc-diff
      "gg" 'xref-find-definitions
      "gr" 'xref-find-references)


    (require 'transient)
    ;; file keymaps
    (transient-define-prefix my/file-command
      "Files"
      [["Find"
        ("f" "find-file" find-file)
        ("r" "find recent file" consult-recent-file)
        ("L" "locate file" consult-locate)
        ("d" "open directory" my-open-current-directory)
        ("ed" "find emacs init file" open-my-init-file)]
       ["CRUD"
        ("s" "Save" save-buffer)
        ("j" "Dired jump" dired-jump)
        ;; ("y" "Copy Filename" my/copy-current-filename-to-clipboard)
        ;; ("r" "Rename" my/rename-current-buffer-file)
        ;; ("k" "Delete" my/delete-file-and-buffer)
        ;; ("e" "Exec shell" my/exec-shell-on-buffer)
        ]])

    ;; buffer keymap
    (transient-define-prefix my/buffer-command
      "Buffer"
      [["Find"
        ("b" "switch buffer" switch-to-buffer)
        ]
       ["CRUD"
        ("k" "kill buffer" kill-buffer)
        ("d" "kill this buffer" kill-this-buffer)]])
    ;; workspace keymaps

    ;; window keymaps

    ;; toggle keymaps

    ;; git keymaps

    ;; search keymaps

    ;; project keymaps
    (transient-define-prefix my/project-command
      "Project"
      [["Find"
        ("f" "File" project-find-file)
        ("r" "Recentf" consult-recent-file)
        ("s" "Search" consult-ripgrep)
        ("d" "Dired" project-dired)
        ("b" "buffer" consult-buffer)]
       ["Progn"
        ("e" "Eshell" project-eshell)
        ;; ("m" "Makefile" my/project-run-makefile-target)
        ("c" "Compile" project-compile)
        ;; ("t" "ciTre" my/project-citre)
        ]
       ["Manage"
        ("p" "Project" project-switch-project)
        ;; ("i" "Info" my/project-info)
        ;; ("a" "Add" my/project-add)
        ;; ("n" "New" my/project-new-root)
        ("x" "Xelete" project-forget-project)
        ]])))

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
