;;; init-programming.el -*- lexical-binding: t no-byte-compile: t -*-

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

(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))


;;; Tree-sitter support
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29
(use-package treesit
  :ensure nil
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :custom (major-mode-remap-alist
           '((c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (csharp-mode     . csharp-ts-mode)
             (conf-toml-mode  . toml-ts-mode)
             (css-mode        . css-ts-mode)
             (java-mode       . java-ts-mode)
             (javascript-mode . js-ts-mode)
             (js-json-mode    . json-ts-mode)
             (python-mode     . python-ts-mode)
             (ruby-mode       . ruby-ts-mode)
             (sh-mode         . bash-ts-mode)))
  :config
  (add-to-list 'auto-mode-alist '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))



(use-package reformatter
  :ensure t
  :config
  (reformatter-define black :program "black" :args '("-") :group 'reformatter)
  (reformatter-define blue :program "blue" :args '("-") :group 'reformatter)
  (reformatter-define js-beautify :program "js-beautify" :group 'reformatter)
  (reformatter-define html-beautify :program "html-beautify" :group 'reformatter)
  (reformatter-define css-beautify :program "css-beautify" :group 'reformatter)
  (reformatter-define hindent :program "hindent" :lighter " Hin" :group 'reformatter)
  (reformatter-define ormolu :program "ormolu" :lighter " Orm"
    :args `("--stdin-input-file" ,buffer-file-name) :group 'reformatter))

(use-package apheleia
  :bind ("C-c f" . apheleia-format-buffer)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(isort black)))

(provide 'init-programming)
