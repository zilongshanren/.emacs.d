;;; init-packages.el -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2021-2022 zilongshanren

;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/emacs.d


;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
(require 'cl-lib)

 (defvar my/packages '(
		       ;; --- Auto-completion ---
		       ;; --- Better Editor ---

		       ;; --- Major Mode ---
		       js2-mode
		       ;; --- Minor Mode ---
		       nodejs-repl
		       ;; --- Themes ---
		       yasnippet

		       js2-refactor
		       lsp-mode
		       csharp-mode
		       json-mode
		       flycheck

		       ;; solarized-theme
		       ) "Default packages")

 (setq package-selected-packages my/packages)

     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg)))



(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)



(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
;; (add-hook 'csharp-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))








(use-package magit
  :config
  (evil-add-hjkl-bindings magit-status-mode-map
    'emacs))

(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("Puffer" . json-mode))
  :hook (json-mode . flycheck-mode)
  :config)

(provide 'init-packages)
