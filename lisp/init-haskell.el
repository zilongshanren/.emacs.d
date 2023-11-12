
(use-package haskell-mode
  :ensure t
  :defer t
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-auto-insert-module-template)))


(provide 'init-haskell)
