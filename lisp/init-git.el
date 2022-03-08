;;; init-git.el -*- lexical-binding: t no-byte-compile: t -*-

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

(use-package magit
  :commands (magit-status)
  :config
  (with-eval-after-load 'evil
    (evil-add-hjkl-bindings magit-status-mode-map
      'emacs
      (kbd "l") 'nil
      (kbd "h") 'nil
      (kbd "C-u") 'evil-scroll-up
      (kbd "C-d") 'evil-scroll-down
      (kbd "K") 'magit-discard
      (kbd "s-1") 'magit-jump-to-unstaged
      (kbd "s-2") 'magit-jump-to-untracked)))


(provide 'init-git)
