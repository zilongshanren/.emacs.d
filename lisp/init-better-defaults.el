
(setq ring-bell-function 'ignore)


(global-auto-revert-mode t)

(global-linum-mode t)


(abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ;; signature
					    ("8zl" "zilongshanren")
					    ;; Microsoft
					    ("8ms" "Macrosoft")
					    ))

(setq make-backup-files nil)

(setq auto-save-default nil)



(recentf-mode 1)			
(setq recentf-max-menu-items 25)


(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(delete-selection-mode t)

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer. "
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region."))
      (progn
	(indent-buffer)
	(message "Indented buffer.")))))



(provide 'init-better-defaults)
