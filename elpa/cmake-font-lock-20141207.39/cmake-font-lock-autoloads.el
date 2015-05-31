;;; cmake-font-lock-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "cmake-font-lock" "cmake-font-lock.el" (21866
;;;;;;  28988 0 0))
;;; Generated autoloads from cmake-font-lock.el

(autoload 'cmake-font-lock-activate "cmake-font-lock" "\
Activate advanced CMake colorization.

To activate this every time a CMake file is opened, use the following:

    (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

\(fn)" t nil)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cmake-font-lock-autoloads.el ends here
