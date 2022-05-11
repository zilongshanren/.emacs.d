;;; init-flycheck.el -*- lexical-binding: t no-byte-compile: t -*-

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

(use-package flycheck
  :ensure t
  :config
  (when sys/win32p
    (setq flycheck-check-syntax-automatically '(save)))

  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (flycheck-define-checker javascript-eslint
    "A Javascript syntax and style checker using eslint.

See URL `https://eslint.org/'."
    :command ("eslint" "--format=json"
              (option-list "--rulesdir" flycheck-eslint-rules-directories)
              (eval flycheck-eslint-args)
              "--stdin" "--stdin-filename" source-original)
    :standard-input t
    :error-parser flycheck-parse-eslint
    :enabled (lambda () (flycheck-eslint-config-exists-p))
    :modes (genehack-vue-mode js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode
                              typescript-mode)
    :working-directory flycheck-eslint--find-working-directory
    :verify
    (lambda (_)
      (let* ((default-directory
               (flycheck-compute-working-directory 'javascript-eslint))
             (have-config (flycheck-eslint-config-exists-p)))
        (list
         (flycheck-verification-result-new
          :label "config file"
          :message (if have-config "found" "missing or incorrect")
          :face (if have-config 'success '(bold error))))))
    :error-explainer
    (lambda (err)
      (let ((error-code (flycheck-error-id err))
            (url "https://eslint.org/docs/rules/%s"))
        (and error-code
             ;; skip non-builtin rules
             (not ;; `seq-contains-p' is only in seq >= 2.21
              (with-no-warnings (seq-contains error-code ?/)))
             `(url . ,(format url error-code))))))

  ;; install json link with commands:  npm install -g mwks-jsonlint --force
  ;; don't use original jsonlint which doesn't respect comments in json
  (flycheck-define-checker json-jsonlint
    "A JSON syntax and style checker using jsonlint.

See URL `https://github.com/zaach/jsonlint'."
    ;; We can't use standard input for jsonlint, because it doesn't output errors
    ;; anymore when using -c -q with standard input :/
    :command ("jsonlint" "-c" "-C" "-q" source)
    :error-patterns
    ((error line-start
            (file-name)
            ": line " line
            ", col " column ", "
            (message) line-end))
    :error-filter
    (lambda (errors)
      (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
    :modes json-mode))

(provide 'init-flycheck)
