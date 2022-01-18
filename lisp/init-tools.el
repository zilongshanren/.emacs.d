;;; init-tools.el -*- lexical-binding: t no-byte-compile: t -*-

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

(use-package quelpa)

(unless (package-installed-p 'quelpa-use-package)
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git")))

(use-package quelpa-use-package
  :init
  (setq quelpa-use-package-inhibit-loading-quelpa t)
  :demand t)

(use-package highlight-global
  :quelpa (highlight-global :fetcher github :repo "glen-dai/highlight-global"))


(use-package symbol-overlay)
(use-package discover-my-major)
(use-package visual-regexp)
(use-package visual-regexp-steroids)
(use-package youdao-dictionary)
(use-package cal-china-x)
(use-package org-super-agenda)
;; (use-package rime)


(use-package expand-region
  :config
  (defadvice er/prepare-for-more-expansions-internal
      (around helm-ag/prepare-for-more-expansions-internal activate)
    ad-do-it
    (let ((new-msg (concat (car ad-return-value)
                           ", H to highlight in buffers"
                           ", / to search in project, "
                           "f to search in files, "
                           "b to search in opened buffers"))
          (new-bindings (cdr ad-return-value)))
      (cl-pushnew
       '("H" (lambda ()
               (call-interactively
                'zilongshanren/highlight-dwim)))
       new-bindings)
      (cl-pushnew
       '("/" (lambda ()
               (call-interactively
                'my/search-project-for-symbol-at-point)))
       new-bindings)
      (cl-pushnew
       '("f" (lambda ()
               (call-interactively
                'find-file)))
       new-bindings)
      (cl-pushnew
       '("b" (lambda ()
               (call-interactively
                'consult-line)))
       new-bindings)
      (setq ad-return-value (cons new-msg new-bindings))))

  )

(provide 'init-tools)
