;;; flymake-posframe.el --- Display flymake diagnostics at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Aya Igarashi

;; Author: Aya Igarashi <ladiclexxx@gmail.com>
;; URL: https://github.com/Ladicle/flymake-posframe
;; Keywords: convenience, languages, tools
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (posframe "0.4.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Display flymake message at point using a posframe.
;; Check out the README for more information.

;;; Code:

(require 'flymake)
(require 'posframe)
(require 'subr-x)

(defvar flymake-posframe-delay 0.5)
(defvar flymake-posframe-buffer "*flymake-posframe*")
(defvar flymake-posframe--last-diag nil)
(defvar flymake-posframe--timer nil)

(defun flymake-posframe-hide ()
  (posframe-hide flymake-posframe-buffer))

(defun flymake-posframe-display ()
  (when flymake-mode
    (if-let (diag (and flymake-mode
                       (get-char-property (point) 'flymake-diagnostic)))
        (unless (and (eq diag flymake-posframe--last-diag)
                     (frame-visible-p (buffer-local-value 'posframe--frame (get-buffer flymake-posframe-buffer))))
          (setq flymake-posframe--last-diag diag)
          (posframe-show
           flymake-posframe-buffer
           :string (propertize (concat "➤ " (flymake--diag-text diag))
                               'face
                               (case (flymake--diag-type diag)
                                 (:error 'error)
                                 (:warning 'warning)
                                 (:note 'info)))))
      (flymake-posframe-hide))))

(defun flymake-posframe-set-timer ()
  (when flymake-posframe--timer
    (cancel-timer flymake-posframe--timer))
  (setq flymake-posframe-timer
        (run-with-idle-timer flymake-posframe-delay nil #'flymake-posframe-display)))

(define-minor-mode flymake-posframe-mode
  "Minor mode for displaying flymake diagnostics at point."
  :lighter nil
  :group flymake-posframe
  (cond
   (flymake-posframe-mode
    (add-hook 'post-command-hook #'flymake-posframe-display nil 'local))
   (t
    (remove-hook 'post-command-hook #'flymake-posframe-display 'local))))

(provide 'flymake-posframe)
;;; flymake-posframe.el ends here
