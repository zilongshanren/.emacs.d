;;; init-csharp.el -*- lexical-binding: t no-byte-compile: t -*-

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

(use-package csharp-mode
  :ensure t
  :mode (("\\.cs\\'" . csharp-mode))
  :config
  (add-to-list 'compilation-error-regexp-alist-alist
               '(my-csharp
                 "^\\(.+\\)(\\([1-9][0-9]+\\),\\([0-9]+\\)): \\(?:\\(warning\\)\\|error\\)?"
                 1 2 3 (4)))
  (add-to-list 'compilation-error-regexp-alist 'my-csharp)
  (defun my-csharp-repl ()
    "Switch to the CSharpRepl buffer, creating it if necessary."
    (interactive)
    (if-let ((buf (get-buffer "*CSharpRepl*")))
        (pop-to-buffer buf)
      (when-let ((b (make-comint "CSharpRepl" "csharp")))
        (switch-to-buffer-other-window b))))
  (define-key csharp-mode-map (kbd "C-c C-z") 'my-csharp-repl))


(provide 'init-csharp)
