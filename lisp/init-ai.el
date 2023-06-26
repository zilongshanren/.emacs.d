

(use-package org-ai
  :commands (org-ai-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  ;; if you are using yasnippet and want `ai` snippets
  (org-ai-install-yasnippets))

;; (require 'mind-wave)

(require 'chatgpt-shell)

(setq chatgpt-shell-openai-key
      (plist-get (car (auth-source-search :host "api.openai.com"))
                 :secret))


(provide 'init-ai)
