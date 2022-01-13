;;;;  -*- lexical-binding: t; -*-
;; (package-initialize)

;; time the loading of the .emacs
;; keep this on top of your .emacs
(defvar *emacs-load-start* (current-time))

(setq package-native-compile t)



(defun anarcat/time-to-ms (time)
  (+ (* (+ (* (car time) (expt 2 16)) (car (cdr time))) 1000000) (car (cdr (cdr time)))))
  
(defun anarcat/display-timing ()
  (message ".emacs loaded in %fms" (/ (- (anarcat/time-to-ms (current-time)) (anarcat/time-to-ms *emacs-load-start*)) 1000000.0)))

(add-hook 'after-init-hook 'anarcat/display-timing t)

(require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/") t)

(setq create-lockfiles nil)
(setq package-archives
      '(("gnu-cn"   . "https://elpa.zilongshanren.com/gnu/") 
	("melpa-cn" . "https://elpa.zilongshanren.com/melpa/")))
			 

(unless package--initialized (package-initialize))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
  
(eval-when-compile
  (require 'use-package))
  
(setq use-package-always-ensure t)
		 

(add-to-list 'load-path (concat user-emacs-directory "lisp")) 

(require 'cl-lib)

 (defvar my/packages '(
		;; --- Auto-completion ---
		company
		;; --- Better Editor ---
		hungry-delete
		swiper
		counsel
		smartparens
		lispy
		lispyville
		magit
		;; --- Major Mode ---
		js2-mode
		consult
		;; --- Minor Mode ---
		nodejs-repl
		exec-path-from-shell
		;; --- Themes ---
		citre
		monokai-theme
		popwin
		org-pomodoro
		yasnippet
		evil
		vertico
		consult-projectile
		evil-leader
		window-numbering
		evil-surround
		evil-nerd-commenter
		which-key
		js2-refactor
		lsp-mode
		csharp-mode
		eglot
		;; solarized-theme
		) "Default packages")

 (setq package-selected-packages my/packages)


	 
     
	 
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg)))
	 
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))	 

(defun open-my-init-file()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory )))

;; not compatable with consult
;; (ivy-mode 1)

(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-keybindings)

(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))

(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(load-file custom-file)


(defvar my-mode-line-coding-format
      '(:eval
        (let* ((code (symbol-name buffer-file-coding-system))
               (eol-type (coding-system-eol-type buffer-file-coding-system))
               (eol (if (eq 0 eol-type) "UNIX"
                      (if (eq 1 eol-type) "DOS"
                        (if (eq 2 eol-type) "MAC"
                          "???")))))
          (concat code " " eol " "))))

(put 'my-mode-line-coding-format 'risky-local-variable t)

(require 'cl-lib)
(setq-default mode-line-format (cl-substitute
                                'my-mode-line-coding-format
                                'mode-line-mule-info
                                mode-line-format)) 

;; (use-package eglot
;;   :commands (eglot eglot-ensure)
;;   :hook ((python-mode . eglot-ensure)
;;          (csharp-mode . eglot-ensure))
;;   :config
;;   (progn
;;     (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
;;     (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
;;     (define-key eglot-mode-map (kbd "C-c e h") 'eglot-help-at-point)
;;     (add-to-list 'eglot-server-programs
;;                  `(csharp-mode . ("c:/Users/lionqu/.emacs.d/.cache/lsp/omnisharp-roslyn/latest/omnisharp-roslyn/OmniSharp.exe" "-lsp")))))
;; (require 'nox)

;; (dolist (hook (list
;;                'js-mode-hook
;;                'rust-mode-hook
;;                'python-mode-hook
;;                'ruby-mode-hook
;;                'java-mode-hook
;;                'sh-mode-hook
;;                'php-mode-hook
;;                'c-mode-common-hook
;;                'c-mode-hook
;;                'csharp-mode-hook
;;                'c++-mode-hook
;;                'haskell-mode-hook
;;                ))
;;   (add-hook hook '(lambda () (nox-ensure))))

;; (add-to-list 'nox-server-programs
;;                  `(csharp-mode . ("c:/Users/lionqu/.emacs.d/.cache/lsp/omnisharp-roslyn/latest/omnisharp-roslyn/OmniSharp.exe" "-lsp")))
;; (require 'nox)

;; (dolist (hook (list
;;                'js-mode-hook
;;                'rust-mode-hook
;;                'python-mode-hook
;;                'ruby-mode-hook
;;                'java-mode-hook
;;                'sh-mode-hook
;;                'php-mode-hook
;;                'c-mode-common-hook
;;                'c-mode-hook
;;                'csharp-mode-hook
;;                'c++-mode-hook
;;                'haskell-mode-hook
;;                ))
;;   (add-hook hook '(lambda () (nox-ensure))))

;; (add-to-list 'nox-server-programs
;;                  `(csharp-mode . ("c:/Users/lionqu/.emacs.d/.cache/lsp/omnisharp-roslyn/latest/omnisharp-roslyn/OmniSharp.exe" "-lsp")))

(require 'citre)
(require 'citre-config)
;; (setq orderless-component-separator "[ &]")
;; 此处配置省略...


;; 此处配置省略...

(setq completion-styles '(orderless partial-completion))

(with-eval-after-load 'company
  (company-flx-mode +1))

(require 'general)
(general-emacs-define-key 'global [remap xref-find-references] 'consult-xref)
(define-key company-mode-map [remap completion-at-point] #'consult-company)
