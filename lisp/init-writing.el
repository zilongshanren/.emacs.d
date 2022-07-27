;;; init-writing.el -*- lexical-binding: t no-byte-compile: t -*-

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

(use-package rime
  :init
  (setq default-input-method "rime")
  :config
  (progn (set-face-attribute 'rime-default-face nil :foreground "#839496" :background "#073642")
         (setq rime-disable-predicates
               '(rime-predicate-evil-mode-p
                 rime-predicate-after-alphabet-char-p
                 rime-predicate-punctuation-line-begin-p
                 rime-predicate-prog-in-code-p))
         (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
         (setq rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-mac/emacs-28.1-mac-9.0/include")
         (setq rime-show-candidate 'posframe)
         (setq rime-share-data-dir "~/Library/Rime")
         (setq rime-user-data-dir "~/Library/Rime")
         (if sys/win32p
             (progn
               (setq rime-share-data-dir "C:\\Users\\lionqu\\AppData\\Roaming\\Rime")
               (setq rime-user-data-dir "C:\\Users\\lionqu\\AppData\\Roaming\\Rime")
               ))
         (setq rime-posframe-properties
               (list :background-color "#073642"
                     :foreground-color "#839496"
                     :internal-border-width 1))))

(use-package pyim
  :ensure t
  :commands (pyim-cregexp-build)
  :init
  (defun eh-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
      (pyim-cregexp-build result)))


  (defun toggle-chinese-search ()
    (interactive)
    (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
        (advice-add 'orderless-regexp :around #'eh-orderless-regexp)
      (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  (defun disable-py-search (&optional args)
    (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
        (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  ;; (advice-add 'exit-minibuffer :after #'disable-py-search)
  (add-hook 'minibuffer-exit-hook 'disable-py-search)

  (global-set-key (kbd "s-p") 'toggle-chinese-search)
  ;; use #$#pyim to search chinese and also es.exe locate 子龙
  )


(use-package ispell-minor-mode
  :ensure nil
  :config
  (advice-add 'ispell-lookup-words :around
              (lambda (orig &rest args)
                (shut-up (apply orig args)))))

(use-package flyspell-correct
  :ensure t
  :init

  )

(use-package ispell
  :ensure nil
  :init
  (if sys/win32p
      (setq ispell-program-name "aspell"))
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (ispell-change-dictionary "american" t))

(use-package corfu-english-helper
  :ensure nil
  :commands toggle-corfu-english-helper)

(use-package olivetti
  :init
  (setq olivetti-body-width nil)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (olivetti-mode t)
      (progn
        (olivetti-mode 0))))
  :bind
  (("<f9>" . distraction-free)))


(use-package ox-hugo
  :ensure t                             ;Auto-install the package from Melpa
  :pin melpa ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox
  :commands org-hugo-export-to-md)

(use-package pangu-spacing
  :defer t
  :init (progn (global-pangu-spacing-mode 1)

               ;; Always insert `real' space in org-mode.
               (add-hook 'org-mode-hook
                         (lambda ()
                           (setq-local pangu-spacing-real-insert-separtor t)))))

(provide 'init-writing)
