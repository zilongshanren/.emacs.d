;;; init.el -*- lexical-binding: t no-byte-compile: t -*-

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




(use-package which-key
  :hook (after-init . which-key-mode)
  :ensure t
  :init
  (setq which-key-side-window-location 'bottom))




(global-set-key "\C-s" 'consult-line)


(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key (kbd "<f2>") 'open-my-init-file)


(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "s-1") 'lispy-describe-inline)

;; mimic macos keybindgs
(when (or sys/win32p sys/mac-x-p)
    (progn
      (global-set-key (kbd "s-x") 'kill-region)
      (global-set-key (kbd "s-c") 'kill-ring-save)
      (global-set-key (kbd "s-v") 'yank)
      (global-set-key (kbd "s-z") 'undo)
      (global-set-key (kbd "s-l") 'goto-line)
      (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
      (global-set-key (kbd "s-a") 'mark-whole-buffer)
      (global-set-key (kbd "s-s") 'save-buffer)))




(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(global-set-key (kbd "s-/") 'hippie-expand)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))


;; r aka remember
(global-set-key (kbd "C-c r") 'org-capture)

(global-set-key (kbd "C-c t i") 'my-toggle-web-indent)

(js2r-add-keybindings-with-prefix "C-c C-m")


(global-set-key (kbd "M-s e") 'iedit-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous))

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-h a") 'apropos)


(global-set-key (kbd "s-d") 'zilongshanren/my-mc-mark-next-like-this)
(global-set-key (kbd "C-c l") 'zilongshanren/insert-chrome-current-tab-url)



(use-package general
	:init
	(general-emacs-define-key 'global [remap xref-find-references] 'consult-xref)
    (general-emacs-define-key 'global [remap imenu] 'consult-imenu)
    (general-emacs-define-key 'global [remap apropos] 'consult-apropos)
	)

(provide 'init-keybindings)
