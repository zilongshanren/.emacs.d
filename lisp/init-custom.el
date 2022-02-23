;;; init-ui.el -*- lexical-binding: t no-byte-compile: t -*-

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

;;; Commentary:
;;
;; Customization.
;;

;;; Code:


(defcustom zilongshanren-server t
  "Enable `server-mode' or not."
  :group 'zilongshanren
  :type 'boolean)

;; Emacs Lisp Package Archive (ELPA)
;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom zilongshanren-package-archives-alist
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    `(,(cons 'melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
      ,(cons 'bfsu
             `(,(cons "gnu"   (concat proto "://mirrors.bfsu.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.bfsu.edu.cn/elpa/melpa/"))))
      ,(cons 'emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.zilongshanren.com/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.zilongshanren.com/melpa/"))))
      ,(cons 'netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
      ,(cons 'ustc
             `(,(cons "gnu"   (concat proto "://mirrors.ustc.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.ustc.edu.cn/elpa/melpa/"))))
      ,(cons 'tencent
             `(,(cons "gnu"   (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))))
      ,(cons 'tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))))
  "The package archives group list."
  :group 'zilongshanren
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom zilongshanren-package-archives 'emacs-china
  "Set package archives from which to fetch."
  :group 'zilongshanren
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value zilongshanren-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    zilongshanren-package-archives-alist)))

(defcustom zilongshanren-theme-alist
  '((default . doom-one)
    (pro     . doom-monokai-pro)
    (dark    . doom-dark+)
    (light   . doom-one-light)
    (warm    . doom-solarized-light)
    (cold    . doom-city-lights)
    (day     . doom-tomorrow-day)
    (night   . doom-tomorrow-night))
  "List of themes mapped to internal themes."
  :group 'zilongshanren
  :type '(alist :key-type (symbol :tag "Theme")
                :value-type (symbol :tag "Internal theme")))

(defcustom zilongshanren-auto-themes '(("8:00"  . doom-one-light)
				                 ("19:00" . doom-one))
  "List of themes mapped to the time they should be loaded.

The keywords `:sunrise' and `:sunset' can be used for the time
if `calendar-latitude' and `calendar-longitude' are set.
For example:
  '((:sunrise . doom-one-light)
    (:sunset  . doom-one))"
  :group 'zilongshanren
  :type '(alist :key-type (string :tag "Time")
                :value-type (symbol :tag "Theme")))

(defvar blog-admin-dir ""
      "blog-admin files location")

(if sys/win32p
    (setq
     blog-admin-dir "d:/zilongshanren.com")
  (setq
   blog-admin-dir "~/zilongshanren.com"))



;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
