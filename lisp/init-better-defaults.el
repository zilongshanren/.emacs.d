;;;;  -*- lexical-binding: t; -*-

(require 'init-funcs)

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches
        "-alh")

  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "open")
          ("\\.docx\\'" "open")
          ("\\.\\(?:djvu\\|eps\\)\\'" "open")
          ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
          ("\\.\\(?:xcf\\)\\'" "open")
          ("\\.csv\\'" "open")
          ("\\.tex\\'" "open")
          ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
           "open")
          ("\\.\\(?:mp3\\|flac\\)\\'" "open")
          ("\\.html?\\'" "open")
          ("\\.md\\'" "open")))




  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always))





(use-package dired-x
  :ensure nil
  :demand t
  :commands (dired-jump)
  :config
  (setq dired-omit-files
        (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$")))

(when sys/macp
  (use-package dired-quick-sort
    :ensure t
    :init
    (if (string-match-p "x86_64" system-configuration)
        (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls")
      (setq insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls"))
    (dired-quick-sort-setup)))


(use-package smartparens
  :init
  (smartparens-global-mode t)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  :config
    (sp-with-modes
        '(c++-mode objc-mode c-mode)
      (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package hungry-delete
  :init
  (global-hungry-delete-mode))

(global-auto-revert-mode t)

(provide 'init-better-defaults)
