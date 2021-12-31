(with-eval-after-load 'org
  (setq org-agenda-files '("~/.emacs.d")
	(add-hook 'org-mode-hook 'org-pomodoro)
	)

(setq org-src-fontify-natively t)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.emacs.d/gtd.org" "工作安排")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)
	("c" "Chrome" entry (file+headline "~/.emacs.d/gtd.org" "Quick notes")
               "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
               :empty-lines 1)
	))
  )




(provide 'init-org)
