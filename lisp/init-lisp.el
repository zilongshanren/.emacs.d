;;; init-lisp.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package lispy
  :ensure t
  :hook (emacs-lisp-mode . lispy-mode)
  :config
  (define-key lispy-mode-map (kbd "e") 'eval-last-sexp))

(use-package lispyville
  :ensure t
  :hook (lispy-mode . lispyville-mode))

(use-package macrostep
  :ensure t)

(provide 'init-lisp)
