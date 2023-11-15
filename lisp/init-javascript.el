;;; init-javascript.el -*- lexical-binding: t no-byte-compile: t -*-

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

(use-package json-mode
  :init
  ;; https://www.emacswiki.org/emacs/AutoModeAlist
  ;; \\' means the end of the file
  (add-to-list 'auto-mode-alist '("Puffer\\'" . json-mode))
  :hook (json-mode . flycheck-mode)
  :config)



(use-package js2-mode
  :ensure t
  :config
  (defun js2-imenu-make-index ()
    (interactive)
    (save-excursion
      ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
      (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                 ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                 ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                 ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                 ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                 ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                                 ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                                 ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                                 ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
                                 ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
                                 ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq imenu-create-index-function 'js2-imenu-make-index)))



  (add-hook 'js2-mode-hook 'flycheck-mode))


(use-package typescript-ts-mode
  :init
  ;; Associate cts files with `typescript-ts-mode'.
  (add-to-list 'auto-mode-alist (cons "\\.cts\\'" 'typescript-ts-mode))
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  :custom (typescript-ts-mode-indent-offset 4)
  :ensure nil
  :config
  (define-key typescript-ts-mode-map (kbd "RET") 'av/auto-indent-method-maybe))

(provide 'init-javascript)
