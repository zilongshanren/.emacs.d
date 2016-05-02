;;; powerline-evil.el --- Utilities for better Evil support for Powerline

;; Copyright (C) 2014 Chris Johnson

;; Author: Chris Johnson <raugturi@gmail.com>
;; Version: 1.0
;; Package-Requires: ((evil "1.0.8") (powerline "2.3"))
;; Keywords: evil, mode-line, powerline
;; URL: http://github.com/raugturi/powerline-evil/

;;; Commentary:
;;
;; Utilities for better Evil support for Powerline and a few extra themes.
;;

;;; Code:

(require 'evil)
(require 'powerline)
(require 'powerline-evil-themes)

(defcustom powerline-evil-tag-style 'visual-expanded
  "The style to use for displaying the evil state tag.

Valid Values: standard, verbose, visual-expanded"
  :group 'powerline
  :type '(choice (const standard)
                 (const verbose)
                 (const visual-expanded)))

(defface powerline-evil-base-face
  '((t (:foreground "white" :inherit mode-line)))
  "Base face for powerline evil faces."
  :group 'powerline)

(defface powerline-evil-normal-face
  '((t (:background "green" :inherit powerline-evil-base-face)))
  "Powerline face for evil NORMAL state."
  :group 'powerline)

(defface powerline-evil-insert-face
  '((t (:background "blue" :inherit powerline-evil-base-face)))
  "Powerline face for evil INSERT state."
  :group 'powerline)

(defface powerline-evil-visual-face
  '((t (:background "orange" :inherit powerline-evil-base-face)))
  "Powerline face for evil VISUAL state."
  :group 'powerline)

(defface powerline-evil-operator-face
  '((t (:background "cyan" :inherit powerline-evil-operator-face)))
  "Powerline face for evil OPERATOR state."
  :group 'powerline)

(defface powerline-evil-replace-face
  '((t (:background "red" :inherit powerline-evil-base-face)))
  "Powerline face for evil REPLACE state."
  :group 'powerline)

(defface powerline-evil-motion-face
  '((t (:background "magenta" :inherit powerline-evil-base-face)))
  "Powerline face for evil MOTION state."
  :group 'powerline)

(defface powerline-evil-emacs-face
  '((t (:background "violet" :inherit powerline-evil-base-face)))
  "Powerline face for evil EMACS state."
  :group 'powerline)


;;;###autoload
(defun powerline-evil-face ()
  "Function to select appropriate face based on `evil-state'."
  (let* ((face (intern (concat "powerline-evil-" (symbol-name evil-state) "-face"))))
    (if (facep face) face nil)))

(defun powerline-evil-tag ()
  "Get customized tag value for current evil state."
  (let* ((visual-block (and (evil-visual-state-p)
                            (eq evil-visual-selection 'block)))
         (visual-line (and (evil-visual-state-p)
                           (eq evil-visual-selection 'line))))
    (cond ((eq powerline-evil-tag-style 'visual-expanded)
           (cond (visual-block " +V+ ")
                 (visual-line " -V- ")
                 (t evil-mode-line-tag)))
          ((eq powerline-evil-tag-style 'verbose)
           (upcase (concat (symbol-name evil-state)
                           (cond (visual-block " BLOCK")
                                 (visual-line " LINE")))))
          (t evil-mode-line-tag))))

(provide 'powerline-evil)
;;; powerline-evil.el ends here
