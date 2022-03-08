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


;; (use-package dirvish
;;   :custom
;;   (dirvish-bookmarks-alist
;;    '(("h" "~/"                          "Home")
;;      ("d" "~/Downloads/"                "Downloads")
;;      ))
;;   :config
;;   (dirvish-override-dired-mode)
;;   (dirvish-peek-mode)
;;   (setq dirvish-attributes '(vscode-icon file-size))
;;   :hook (dirvish-mode-hook . dirvish-toggle-fullscreen)
;;   :bind
;;   (:map dired-mode-map
;;         ("SPC" . dirvish-show-history)
;;         ("r"   . dirvish-roam)
;;         ("b"   . dirvish-goto-bookmark)
;;         ("f"   . dirvish-file-info-menu)
;;         ("M-a" . dirvish-mark-actions-menu)
;;         ("M-s" . dirvish-setup-menu)
;;         ("M-f" . dirvish-toggle-fullscreen)
;;         ([remap dired-summary] . dirvish-dispatch)
;;         ([remap mode-line-other-buffer] . dirvish-other-buffer)))

;; (use-package vscode-icon
;;   :config
;;   (push '("jpg" . "image") vscode-icon-file-alist))

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
