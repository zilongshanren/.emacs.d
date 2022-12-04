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




(require 'lsp-bridge)
(setq lsp-bridge-enable-log nil)
(global-lsp-bridge-mode)

(defun my/enable-lsp-bridge ()
  (interactive)
  (progn
    ;; (corfu-mode -1)
    ;; (lsp-bridge-mode)

    (setq acm-candidate-match-function 'orderless-flex)

    (define-key evil-motion-state-map "gR" #'lsp-bridge-rename)
    (define-key evil-motion-state-map "gr" #'lsp-bridge-find-references)
    (define-key evil-normal-state-map "gi" #'lsp-bridge-find-impl)
    (define-key evil-motion-state-map "gd" #'lsp-bridge-jump)
    (define-key evil-motion-state-map "gs" #'lsp-bridge-restart-process)
    (define-key evil-normal-state-map "gh" #'lsp-bridge-popup-documentation)
    (define-key evil-normal-state-map "gn" #'lsp-bridge-diagnostic-jump-next)
    (define-key evil-normal-state-map "gp" #'lsp-bridge-diagnostic-jump-prev)
    (define-key evil-normal-state-map "ga" #'lsp-bridge-code-action)
    (define-key evil-normal-state-map "ge" #'lsp-bridge-diagnostic-list)

    (define-key lsp-bridge-mode-map (kbd "s-j") 'lsp-bridge-popup-documentation-scroll-down)
    (define-key lsp-bridge-mode-map (kbd "s-k") 'lsp-bridge-popup-documentation-scroll-up)

    (define-key acm-mode-map (kbd "C-j") 'acm-select-next)
    (define-key acm-mode-map (kbd "C-k") 'acm-select-prev)

    (setq acm-continue-commands '(nil ignore universal-argument universal-argument-more digit-argument
                                      self-insert-command org-self-insert-command
                                      ;; Avoid flashing completion menu when backward delete char
                                      grammatical-edit-backward-delete backward-delete-char-untabify
                                      python-indent-dedent-line-backspace delete-backward-char hungry-delete-backward
                                      "\\`acm-" "\\`scroll-other-window"))
    ;; make acm key mapping the highest precedence
    (setq acm-map-alist (assoc 'acm-mode minor-mode-map-alist))
    (assq-delete-all 'acm-mode minor-mode-map-alist)
    (add-to-list 'minor-mode-map-alist acm-map-alist)

    ))

(add-hook 'lsp-bridge-mode-hook 'my/enable-lsp-bridge)

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
   ((eq major-mode 'lisp-interaction-mode)
    (evil-goto-definition))
   ((eq major-mode 'org-mode)
    (org-agenda-open-link))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))


(evil-define-key 'normal lsp-bridge-ref-mode-map
  (kbd "RET") 'lsp-bridge-ref-open-file-and-stay
  "q" 'lsp-bridge-ref-quit)

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)     ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))




(provide 'init-lsp)
