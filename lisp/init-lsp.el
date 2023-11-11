;;; init-lsp.el -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2021-2023 zilongshanren

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


(define-derived-mode genehack-vue-mode web-mode "ghVue"
    "A major mode derived from web-mode, for editing .vue files with LSP support.")

(defun my-eglot-keybindgs ()
  (define-key evil-motion-state-map "gR" #'eglot-rename)
  (define-key evil-motion-state-map "gr" #'xref-find-references)
  (define-key evil-normal-state-map "gi" #'eglot-find-implementation)
  (define-key evil-motion-state-map "gh" #'eldoc)
  (define-key evil-normal-state-map "ga" #'eglot-code-actions))

(use-package eglot
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . genehack-vue-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (advice-add 'eglot-ensure :after 'my-eglot-keybindgs)
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc)
              ("s-<return>" . eglot-code-actions))
  :hook
  (css-mode . eglot-ensure)
  (js2-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  (web-mode . eglot-ensure)
  (genehack-vue-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  (elixir-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  ;; disable for performance issue, specially for peek framework definition
  ;; (dart-mode . eglot-ensure)
  :config
  (setq eglot-send-changes-idle-time 0.2)
  (setq eldoc-echo-area-use-multiline-p nil)
  (add-to-list 'eglot-server-programs '(genehack-vue-mode "vls"))
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd" "--enable-config")))
  (add-to-list 'eglot-server-programs '(web-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(elixir-mode "~/.emacs.d/elixir-ls/release/language_server.sh"))



  (setq read-process-output-max (* 1024 1024))
  (push :documentHighlightProvider eglot-ignored-server-capabilities)
  (setq eldoc-echo-area-use-multiline-p nil))

  (cl-defmacro eglot-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang string)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "eglot--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((file-name (->> info caddr (alist-get :file))))
             (unless file-name
               (setq file-name (concat default-directory (if (string= ,lang "C") "org-src-babel.c" "org-src-babel.cpp")))
               (write-region (point-min) (point-max) file-name))
             (setq buffer-file-name file-name)
             (eglot-ensure)))
         (put ',intern-pre 'function-documentation
              (format "Enable eglot mode in the buffer of org source block (%s)."
                      (upcase ,lang)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (with-eval-after-load 'org
    (dolist (lang '("C" "C++"))
      (eval `(eglot-org-babel-enable ,lang))))

(use-package consult-eglot
  :ensure t
  :defer t)



(use-package dumb-jump
  :ensure t
  :config (setq dumb-jump-selector 'completion-read))



(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)     ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))




(provide 'init-lsp)
