;;; init-lsp.el -*- lexical-binding: t no-byte-compile: t -*-

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

;; (use-package lsp-mode
;;   :custom
;;   (lsp-completion-provider :none) ;; we use Corfu!

;;   :init
;;   (defun my/orderless-dispatch-flex-first (_pattern index _total)
;;     (and (eq index 0) 'orderless-flex))

;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless-flex)))

;;   ;; Optionally configure the first word as flex filtered.
;;   (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

;;   ;; Optionally configure the cape-capf-buster.
;;   (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

;;   :hook
;;   (lsp-completion-mode . my/lsp-mode-setup-completion))


;; (define-derived-mode genehack-vue-mode web-mode "ghVue"
;;     "A major mode derived from web-mode, for editing .vue files with LSP support.")

;; (use-package eglot
;;   :ensure t
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.vue\\'" . genehack-vue-mode))
;;   (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;;   :bind (:map eglot-mode-map
;;               ("C-c l a" . eglot-code-actions)
;;               ("C-c l r" . eglot-rename)
;;               ("C-c l f" . eglot-format)
;;               ("C-c l d" . eldoc))
;;   :hook (eglot-managed-mode . (lambda () (flymake-mode -1)))
;;   (css-mode . eglot-ensure)
;;   (js2-mode . eglot-ensure)
;;   (js-mode . eglot-ensure)
;;   (web-mode . eglot-ensure)
;;   (csharp-mode . eglot-ensure)
;;   (python-mode . eglot-ensure)
;;   (genehack-vue-mode . eglot-ensure)
;;   :config
;;   (add-to-list 'eglot-server-programs '(genehack-vue-mode "vls"))
;;   (add-to-list 'eglot-server-programs
;;                `(csharp-mode . ("~/Downloads/omnisharp-osx/run" "-lsp")))
;;   (add-to-list 'eglot-server-programs '(web-mode . ("vscode-html-language-server" "--stdio")))

;;   (setq read-process-output-max (* 1024 1024))
;;   (push :documentHighlightProvider eglot-ignored-server-capabilities)
;;   (setq eldoc-echo-area-use-multiline-p nil))


(require 'lsp-bridge)

(setq lsp-bridge-enable-log nil)

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'python-mode-hook
               'ruby-mode-hook
               'rust-mode-hook
               'elixir-mode-hook
               'go-mode-hook
               'haskell-mode-hook
               'haskell-literate-mode-hook
               'dart-mode-hook
               'scala-mode-hook
               'typescript-mode-hook
               'typescript-tsx-mode-hook
               'js2-mode-hook
               'js-mode-hook
               'rjsx-mode-hook
               'tuareg-mode-hook
               'latex-mode-hook
               'Tex-latex-mode-hook
               'texmode-hook
               'context-mode-hook
               'texinfo-mode-hook
               'bibtex-mode-hook
               'clojure-mode-hook
               'clojurec-mode-hook
               'clojurescript-mode-hook
               'clojurex-mode-hook
               'csharp-mode-hook
               'sh-mode-hook
               'web-mode-hook))
  (add-hook hook (lambda ()
                   (setq-local corfu-auto nil)
                   (lsp-bridge-mode)
                   ;; (lsp-bridge-mix-multi-backends)
                   (setq-local evil-goto-definition-functions '(lsp-bridge-jump))
                   )))


(define-key evil-motion-state-map "gR" #'lsp-bridge-rename)
(define-key evil-motion-state-map "gr" #'lsp-bridge-find-references)
(define-key evil-normal-state-map "gi" #'lsp-bridge-find-impl)
(define-key evil-motion-state-map "gd" #'lsp-bridge-jump)
(define-key evil-motion-state-map "gs" #'lsp-bridge-restart-process)
(define-key evil-normal-state-map "gh" #'lsp-bridge-lookup-documentation)

;; 通过Cape融合不同的补全后端，比如lsp-bridge、 tabnine、 file、 dabbrev.
;; (defun lsp-bridge-mix-multi-backends ()
;;   (setq-local completion-category-defaults nil)
;;   (setq-local completion-at-point-functions
;;               (list
;;                (cape-capf-buster (cape-super-capf
;;                                   'lsp-bridge-capf
;;                                   'cape-file
;;                                   'cape-dabbrev)))))

(use-package dumb-jump
  :ensure t)

;; make evil jump & jump back as expected
;; (defun evil-set-jump-args (&rest ns) (evil-set-jump))
;; (advice-add 'lsp-bridge-jump :before #'evil-set-jump-args)
(evil-add-command-properties #'lsp-bridge-jump :jump t)



;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
(defun lsp-bridge-jump ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (evil-goto-definition))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

(defun lsp-bridge-jump-back ()
  (interactive)
  (cond
   (lsp-bridge-mode
    (lsp-bridge-return-from-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))

(evil-define-key 'normal lsp-bridge-ref-mode-map
  (kbd "RET") 'lsp-bridge-ref-open-file-and-stay
  "q" 'lsp-bridge-ref-quit)

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)     ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))




(provide 'init-lsp)
