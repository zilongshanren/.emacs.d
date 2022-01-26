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


(use-package highlight-global
  :commands (highlight-frame-toggle)
  :quelpa (highlight-global :fetcher github :repo "glen-dai/highlight-global")
  :config
  (progn
    (setq-default highlight-faces
                  '(('hi-red-b . 0)
                    ('hi-aquamarine . 0)
                    ('hi-pink . 0)
                    ('hi-blue-b . 0)))))


(use-package symbol-overlay)


(use-package visual-regexp
  :defer
  :commands (vr/replace vr/query-replace))

(use-package visual-regexp-steroids
  :defer
  :commands (vr/select-replace vr/select-query-replace)
  :init
  (progn
    (define-key global-map (kbd "C-c r") 'vr/replace)
    (define-key global-map (kbd "C-c q") 'vr/query-replace)))

(use-package discover-my-major
  :defer t
    :init
    )


(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point+)
  :init
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point+))

(use-package cal-china-x)
(use-package org-super-agenda)


(use-package expand-region
  :config
  (defadvice er/prepare-for-more-expansions-internal
      (around helm-ag/prepare-for-more-expansions-internal activate)
    ad-do-it
    (let ((new-msg (concat (car ad-return-value)
                           ", H to highlight in buffers"
                           ", / to search in project, "
                           "e iedit mode in functions"
                           "f to search in files, "
                           "b to search in opened buffers"))
          (new-bindings (cdr ad-return-value)))
      (cl-pushnew
       '("H" (lambda ()
               (interactive)
               (call-interactively
                'zilongshanren/highlight-dwim)))
       new-bindings)
      (cl-pushnew
       '("/" (lambda ()
               (interactive)
               (call-interactively
                'my/search-project-for-symbol-at-point)))
       new-bindings)
      (cl-pushnew
       '("e" (lambda ()
               (interactive)
               (call-interactively
                'iedit-mode)))
       new-bindings)
      (cl-pushnew
       '("f" (lambda ()
               (interactive)
               (call-interactively
                'find-file)))
       new-bindings)
      (cl-pushnew
       '("b" (lambda ()
               (interactive)
               (call-interactively
                'consult-line)))
       new-bindings)
      (setq ad-return-value (cons new-msg new-bindings))))

  )

(use-package prodigy
  :commands (prodigy)
  :defer 1
  :config
  (progn

    ;; define service
    (prodigy-define-service
      :name "Hugo Server"
      :command "hugo"
      :args '("server" "-D" "--navigateToChanged" "-t" "even")
      :cwd blog-admin-dir
      :tags '(hugo server)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "hugo Deploy"
      :command "bash"
      :args '("./deploy.sh")
      :cwd blog-admin-dir
      :tags '(hugo deploy)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)))

(use-package pyim
  :demand t
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


(use-package rime
  :init
  (setq default-input-method "rime")
  :config
  (progn (set-face-attribute 'rime-default-face nil :foreground "#839496" :background "#073642")
         (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
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

;;

(use-package ispell-minor-mode
  :ensure nil
  :config
  (advice-add 'ispell-lookup-words :around
              (lambda (orig &rest args)
                (shut-up (apply orig args)))))




(provide 'init-tools)
