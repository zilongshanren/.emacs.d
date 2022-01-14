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

(setq evil-want-C-u-scroll t)

(use-package evil
  :init
  (evil-mode)
  (setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

  )

(use-package evil-leader
  :init
(evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode t))
(global-set-key "\C-s" 'consult-line)


(global-set-key (kbd "<f2>") 'open-my-init-file)


(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "s-1") 'lispy-describe-inline)

;; mimic macos keybindgs
(when sys/win32p
    (progn
      (global-set-key (kbd "s-x") 'kill-region)
      (global-set-key (kbd "s-c") 'kill-ring-save)
      (global-set-key (kbd "s-v") 'yank)
      (global-set-key (kbd "s-z") 'undo)
      (global-set-key (kbd "s-l") 'goto-line)
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





(evil-leader/set-key
  "SPC" 'counsel-M-x
  "ff" 'find-file
  "fr" 'consult-recent-file
  "fs" 'save-buffer
  "bb" 'switch-to-buffer
  "bk" 'kill-buffer
  "pf" 'counsel-git
  "ps" 'consult-ripgrep
  "0" 'select-window-0
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "fj" 'dired-jump
  "w/" 'split-window-right
  "w-" 'split-window-below
  ":" 'counsel-M-x
  "wm" 'delete-other-windows
  "qq" 'save-buffers-kill-terminal
  "sj" 'counsel-imenu
  "bd" 'kill-this-buffer
  "ts" 'flycheck-mode
  "sp" 'consult-ripgrep
  "TAB" 'spacemacs/alternate-buffer
  "fed" 'open-my-init-file
  "hdf" 'describe-function
  "hdv" 'describe-variable
  "hdk" 'describe-key
  "pb" 'consult-buffer
  "gs" 'magit-status
  "gg" 'citre-jump
  "gr" 'citre-peek)

(provide 'init-keybindings)
