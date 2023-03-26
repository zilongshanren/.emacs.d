
(use-package org-ai
  :commands (org-ai-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  ;; if you are using yasnippet and want `ai` snippets
  (org-ai-install-yasnippets))


(provide 'init-ai)
