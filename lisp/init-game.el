;;; init-game.el --- -*- lexical-binding: t; -*-

(use-package gdscript-mode
  :ensure t
  :hook (gdscript-mode . eglot-ensure)
  :init
  (progn
    (setq gdscript-godot-executable "/Applications/Godot.app/Contents/MacOS/Godot")
    (setq gdscript-use-tab-indents t) ;; If true, use tabs for indents. Default: t
    (setq gdscript-indent-offset 4)   ;; Controls the width of tab-based indents
    ;; install pip3 install "gdtoolkit==4.*"
    (setq gdscript-gdformat-save-and-format t) ;; Save all buffers and format them with gdformat anytime Godot executable is run.

    (global-leader
      :major-modes
      '(gdscript-mode t)
      ;;and the keymaps:
      :keymaps
      '(gdscript-mode-map)
      "rr" 'gdscript-godot-run-project
      "=" 'gdscript-format-buffer)))

(require 'glsl-mode)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))


(provide 'init-game)
