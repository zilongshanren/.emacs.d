;; spaceleader-base.el --- use-package keyword for spaceleader.el -*- lexical-binding: t -*-

;; Copyright (C) 2021 Mohsin Kaleem

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: lisp, internal
;; Version: 0.0.3
;; URL: https://github.com/mohkale/spaceleader

;;; License: GPLv3
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Package-Requires: ((emacs "26.1") (general "0.1") (bind-map "1.1") (bind-key "2.4) (dash "2.17"))

;;; Commentary:
;; Adds a `use-package' keyword for `spaceleader-set-keys' and variants.

;;; Code:

(require 'spaceleader)
(require 'use-package)

(defun use-package-normalize/:leader (_name _keyword args)
  args)

(defmacro use-package-leader--plist-pop (list prop &optional default)
  "delete PROP from plist LIST, returning value of PROP.
if PROP isn't in LIST, DEFAULT will be returned."
  `(prog1
       (or (plist-get ,list ,prop) ,default)
     (cl-remf ,list ,prop)))

(defun use-package-leader-format-args (args)
  ;; Properties
  ;;  :defer - when true, leader are only assigned after package load
  ;;  :modes/:mode - when true leaders are only bound in given major modes
  ;;  :minor - when true :modes applies to minor-modes, not majors
  ;;  :prefix - call leader-with-prefix with argument before rendering
  (cl-loop
   with modes = nil
   ;; with minor = nil
   with prefix = nil
   ;; with leader-func = nil
   with res = nil
   for arg in args
   do (let ((major (not (use-package-leader--plist-pop arg :minor))))
        (setq modes (or (use-package-leader--plist-pop arg :modes)
                        (use-package-leader--plist-pop arg :mode))
              prefix (use-package-leader--plist-pop arg :prefix)
              res (if modes
                      (list (if major 'leader-set-keys-for-major-mode 'leader-set-keys-for-mode)
                            modes)
                      ""
                    '(leader-set-keys))
              ;; leader-func '(leader-set-keys-for-mode)
              ))
   when (not arg)
     do (display-warning 'use-package ":leader got no bindings")
   else
     do (setq res `(,@res ,@arg))
   end
   when prefix
     do (setq res `(leader-with-prefix ,prefix ,res))
   end
   collect res))

(defun use-package-handler/:leader (name keyword args rest state &optional _defer)
  (let ((body (use-package-process-keywords name rest state)))
    (if (not args)
        body
      (use-package-concat
       (funcall use-package--hush-function keyword
                (list (cons 'progn (use-package-leader-format-args args))))
       body))))

(defalias 'use-package-normalize/:lazy-leader #'use-package-normalize/:leader)
(defalias 'use-package-handler/:lazy-leader #'use-package-handler/:leader)

;; Add the two new keywords to `use-package-keywords' ensuring their placed
;; at the appropriant position to bind after loading the package or before.
(let ((tail (nthcdr (cl-position :init use-package-keywords) use-package-keywords)))
  (setcdr tail (cons :leader (cdr tail))))
(let ((tail (nthcdr (cl-position :config use-package-keywords) use-package-keywords)))
  (setcdr tail (cons :lazy-leader (cdr tail))))

(provide 'spaceleader-use-package)
