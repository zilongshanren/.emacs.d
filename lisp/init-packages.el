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
		       hungry-delete
		       smartparens
		       lispy
		       lispyville
		       ;; --- Major Mode ---
		       js2-mode
		       ;; --- Minor Mode ---
		       nodejs-repl
		       exec-path-from-shell
		       ;; --- Themes ---
		       citre
		       org-pomodoro
		       yasnippet

		       consult-projectile
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

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :ensure t
  :config
  (progn
     (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive login shell.  A login shell, because my
      ;; environment variables are mostly set in `.zprofile'.
       (setq exec-path-from-shell-arguments '("-l")))

     (exec-path-from-shell-initialize)
    )
  )

(global-hungry-delete-mode)

(smartparens-global-mode t)

(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
(sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)



;; config js2-mode for js files
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode))
       auto-mode-alist))


;; config for web mode
(defun my-web-mode-indent-setup ()
  (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
  (setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
  )

(add-hook 'web-mode-hook 'my-web-mode-indent-setup)


(defun my-toggle-web-indent ()
  (interactive)
  ;; web development
  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
	(setq js-indent-level (if (= js-indent-level 2) 4 2))
	(setq js2-basic-offset (if (= js2-basic-offset 2) 4 2))))

  (if (eq major-mode 'web-mode)
      (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
	     (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
	     (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))

  (setq indent-tabs-mode nil))


;; config for js2-refactor
(add-hook 'js2-mode-hook #'js2-refactor-mode)


(defun js2-imenu-make-index ()
  (interactive)
  (save-excursion
    ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
    (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
			       ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
			       ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))
(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq imenu-create-index-function 'js2-imenu-make-index)))



(add-hook 'js2-mode-hook 'flycheck-mode)

(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode))


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




(require 'citre)
(require 'citre-config)




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
