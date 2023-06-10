;;;;  -*- lexical-binding: t; -*-
(require 'init-funcs)

;;
;;; Packages


(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-'") 'vertico-quick-jump)
  (define-key vertico-map (kbd "C-k") 'vertico-previous)
  (define-key vertico-map [backspace] #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "s-SPC") #'+vertico/embark-preview)

  )


(use-package orderless
  :demand t
  ;;       orderless-component-separator "[ &]")
  ;; ...otherwise find-file gets different highlighting than other commands
  ;; (set-face-attribute 'completions-first-difference nil :inherit nil)
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
                                     (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch))
  )


(use-package consult
  :defer t
  :init
  (if sys/win32p
      (progn
        (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
        (add-to-list 'process-coding-system-alist '("explorer" gbk . gbk))
        (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))))


  (advice-add #'multi-occur :override #'consult-multi-occur)
  (advice-add #'consult-line
              :around
              #'zilongshanren/consult-line
              '((name . "wrapper")))

  :config
  (global-set-key (kbd "M-y") 'consult-yank-pop)
  (setq ;; consult-project-root-function #'doom-project-root
   consult-narrow-key "<"
   consult-line-numbers-widen t
   consult-async-min-input 2
   consult-async-refresh-delay  0.15
   consult-async-input-throttle 0.2
   consult-async-input-debounce 0.1
   consult-line-start-from-top t)



  (consult-customize
   consult-theme
   :preview-key (list (kbd "C-SPC") :debounce 0.5 'any)))


(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("s-d" . consult-dir)))

(use-package consult-flycheck
  :after (consult flycheck))


(use-package embark
  :defer t
  :init
  (setq which-key-use-C-h-commands nil
        ;; press C-h after a prefix key, it shows all the possible key bindings and let you choose what you want
        prefix-help-command #'embark-prefix-help-command)

  (setq
   embark-verbose-indicator-display-action
   '((display-buffer-at-bottom)
     (window-parameters (mode-line-format . none))
     (window-height . fit-window-to-buffer)))

  (define-key minibuffer-local-map (kbd "C-;") 'embark-act)
  (define-key minibuffer-local-map (kbd "C-c C-;") 'embark-export)
  (define-key minibuffer-local-map (kbd "C-c C-e") '+vertico/embark-export-write)

  (with-eval-after-load 'popwin
    (progn
      (push '(occur-mode :position right :width 100) popwin:special-display-config)
      (push '(grep-mode :position right :width 100) popwin:special-display-config)
      (push '(special-mode :position right :width 100) popwin:special-display-config)))

  (global-set-key (kbd "C-;") 'embark-act)

  :config
  (define-key minibuffer-local-map (kbd "C-'") #'embark-become)
  ;; list all the keybindings in this buffer
  (global-set-key (kbd "C-h B") 'embark-bindings)
  ;; add the package! target finder before the file target finder,
  ;; so we don't get a false positive match.
  :config
  (define-key embark-identifier-map "R" #'consult-ripgrep)
  (define-key embark-identifier-map (kbd "C-s") #'consult-line)
  (define-key embark-region-map "D" 'youdao-dictionary-search-async)

  (define-key embark-file-map (kbd "E") #'consult-directory-externally)
  (define-key embark-file-map (kbd "U") #'consult-snv-unlock)
  (define-key embark-file-map (kbd "H") #'my-calculate-file-md5)
  )


(use-package marginalia
  :hook (after-init . marginalia-mode)
  :init
  :config)


(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))


(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))




(provide 'init-completion)
