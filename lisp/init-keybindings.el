;;; init.el -*- lexical-binding: t no-byte-compile: t -*-

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




(use-package which-key
  :hook (after-init . which-key-mode)
  :ensure t
  :init
  (setq which-key-side-window-location 'bottom))




(global-set-key "\C-s" 'consult-line)


(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key (kbd "<f2>") 'open-my-init-file)


(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "s-1") 'lispy-describe-inline)

;; mimic macos keybindgs
(when (or sys/win32p sys/mac-x-p)
    (progn
      (global-set-key (kbd "s-x") 'kill-region)
      (global-set-key (kbd "s-c") 'kill-ring-save)
      (global-set-key (kbd "s-v") 'yank)
      (global-set-key (kbd "s-z") 'undo)
      (global-set-key (kbd "s-l") 'goto-line)
      ;; (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
      (global-set-key (kbd "s-a") 'mark-whole-buffer)
      (global-set-key (kbd "s-s") 'save-buffer)))




(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(define-key global-map (kbd "s-e") 'rime-inline-ascii)
(global-set-key (kbd "s-/") 'hippie-expand)


;; r aka remember
(global-set-key (kbd "C-c r") 'org-capture)




(global-set-key (kbd "M-s e") 'iedit-mode)

(global-set-key (kbd "C-=") 'er/expand-region)


(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-h a") 'apropos)


(global-set-key (kbd "s-r") #'revert-this-buffer)
(global-set-key (kbd "s-d") 'zilongshanren/my-mc-mark-next-like-this)
(global-set-key (kbd "C-c l") 'zilongshanren/insert-chrome-current-tab-url)



(use-package general
  :init
  (with-eval-after-load 'evil
    (general-add-hook 'after-init-hook
                      (lambda (&rest _)
                        (when-let ((messages-buffer (get-buffer "*Messages*")))
                          (with-current-buffer messages-buffer
                            (evil-normalize-keymaps))))
                      nil
                      nil
                      t))

  (general-emacs-define-key 'global [remap imenu] 'consult-imenu)
  (general-emacs-define-key 'global [remap apropos] 'consult-apropos)

  (general-create-definer global-definer
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (global-definer
    "!" 'shell-command
    ":" 'eval-expression
    "SPC" 'execute-extended-command
    "x" 'switch-to-scratch-buffer
    "TAB" 'spacemacs/alternate-buffer
    "'" 'vertico-repeat
    "=" 'indent-buffer
    "u" 'universal-argument
    "v" 'er/expand-region
    "0" 'select-window-0
    "1" 'select-window-1
    "2" 'select-window-2
    "3" 'select-window-3
    ";" 'vterm
    "hdf" 'describe-function
    "hdv" 'describe-variable
    "hdk" 'describe-key
    "qq" 'save-buffers-kill-terminal
    "qR" 'restart-emacs
    "hh" 'zilongshanren/highlight-dwim
    "hc" 'zilongshanren/clearn-highlight
    "sj" 'my/imenu
    "en" 'my-goto-next-error
    "ry" 'consult-yank-pop
    "ep" 'my-goto-previous-error
    "el" 'my-list-errors
    "sp" 'consult-ripgrep
    "oy" 'youdao-dictionary-search-at-point+
    "oo" 'zilongshanren/hotspots
    "gs" 'magit-status
    "gd" 'vc-diff
    "gg" 'xref-find-definitions
    "gr" 'xref-find-references)


  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
         :wrapping global-definer
         :prefix-map ',(intern (concat "+general-global-" name "-map"))
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
        ,@body)))

  (+general-global-menu! "buffer" "b"
    "d" 'kill-current-buffer
    "b" '(switch-to-buffer :which-key "switch buffer")
    "B" '(consult-buffer :which-key "consult buffer")
    "p" 'previous-buffer
    "R" 'rename-buffer
    "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
          :which-key "messages-buffer")
    "n" 'next-buffer
    "i" 'ibuffer
    "f" 'my-open-current-directory
    "k" 'kill-buffer
    "y" 'copy-buffer-name
    "K" 'kill-other-buffers
    )

  (+general-global-menu! "layout" "l"
    "l" 'persp-switch
    "L" 'persp-state-load
    "TAB" 'persp-switch-last
    "A" 'persp-add-buffer
    "s" 'persp-state-save
    "b" 'persp-switch-to-buffer
    "R" 'persp-remove-buffer
    "k" 'persp-kill)

  (+general-global-menu! "file" "f"
    "f" 'find-file
    "r" 'consult-recent-file
    "L" 'consult-locate
    "d" 'consult-dir
    "ed" 'open-my-init-file
    "s" 'save-buffer
    "w" 'sudo-edit
    "S" 'save-some-buffers
    "j"  'dired-jump
    "y" 'copy-file-name
    "R" 'my/rename-current-buffer-file
    "k" 'my/delete-file-and-buffer
    "!" 'my/exec-shell-on-buffer)

  (+general-global-menu! "window" "w"
    "/" 'split-window-right
    "-" 'split-window-below
    "m" 'delete-other-windows
    "u" 'winner-undo
    "z" 'winner-redo)

  (+general-global-menu! "toggle" "t"
    "s" 'flycheck-mode
    "S" 'flyspell-prog-mode
    "e" 'toggle-corfu-english-helper
    "r" 'read-only-mode
    "n" 'my-toggle-line-numbber
    "w" 'distraction-free
    "l" 'my/enable-lsp-bridge
    "k" '+toggle-keycast
    "c" 'global-corfu-mode)

  (+general-global-menu! "project" "p"
    "f" 'project-find-file
    "r" 'consult-recent-file
    "s" 'project-find-regexp
    "d" 'project-dired
    "b" 'consult-project-buffer
    "e" 'project-eshell
    "m" 'my/project-run-makefile-target
    "c" 'project-compile
    "t" 'my/project-citre
    "p" 'project-switch-project
    "i" 'my/project-info
    "a" 'project-remember-projects-under
    "x" 'project-forget-project)

  (general-create-definer global-leader
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix ","
    "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

  ;; mode specific major key
  (global-leader
    :major-modes
    '(org-mode t)
    ;;and the keymaps:
    :keymaps
    '(org-mode-map)
    "p" 'org-pomodoro
    "t" 'org-todo
    "e" 'org-set-effort
    ">" 'org-metaright
    "<" 'org-metaleft
    "J" 'org-metadown
    "K" 'org-metaup
    "T" 'org-set-tags-command
    "l" 'org-toggle-link-display
    "L" 'org-toggle-inline-images
    "I" 'org-clock-in
    "O" 'org-clock-out
    "P" 'org-set-property
    "s" 'org-schedule
    "+" 'org-increase-number-at-point
    "-" 'org-decrease-number-at-point
    "n" 'org-narrow-to-subtree
    "dc" 'org-download-clipboard
    "ds" 'org-download-screenshot
    "w" 'widen)

    (global-leader
      :major-modes
      '(org-agenda-mode t)
      ;;and the keymaps:
      :keymaps
      '(org-agenda-mode-map)
      "d" 'org-agenda-day-view
      "w" 'org-agenda-week-view
      "," 'org-agenda-priority
      "e" 'org-agenda-set-effort
      ":" 'org-agenda-set-tags
      "T" 'org-agenda-show-tags)


  )

(provide 'init-keybindings)
