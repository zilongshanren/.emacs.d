;;; init-ui.el -*- lexical-binding: t no-byte-compile: t -*-

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

(setq inhibit-splash-screen t)
(setq-default cursor-type 'bar)

(setq  initial-frame-alist (quote ((fullscreen . maximized))))

(global-hl-line-mode t)

;; (global-display-line-numbers-mode t)
(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - GNU Emacs %s" emacs-version)))



(use-package doom-themes
  :ensure t
  :init
  (if (display-graphic-p)
      (load-theme 'doom-solarized-light)
    (load-theme 'doom-tomorrow-night)))

(set-face-attribute 'default nil :height 150)

(use-package valign
  :ensure t
  :hook ((markdown-mode org-mode) . valign-mode))


(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-minor-modes t)
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.95))))
  :hook (after-init . doom-modeline-mode))

(use-package visual-fill-column
  :ensure t
  :init
  ;; Configure fill width
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))

(provide 'init-ui)
