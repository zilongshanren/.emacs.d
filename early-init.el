;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 zilongshanren

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

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
;; (push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; TEMP: Explicitly set PATH environment variable and update exec-path to match it
;; otherwise emacs throws jit compilation errors
(setenv "PATH" "/Users/lionqu/.nvm/versions/node/v20.9.0/bin:/Users/lionqu/.nvm/versions/node/v20.9.0/bin:/Applications/Emacs.app/Contents/MacOS/bin/:/Users/lionqu/Library/Python/2.7/bin:/Users/lionqu/.pyenv/shims:/Users/lionqu/.emacs.d/bin:/usr/local/sbin:/Users/lionqu/.nvm/versions/node/v20.9.0/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/Apple/usr/bin:/Users/lionqu/.cargo/bin:/Users/lionqu/.asdf/shims/:/Users/lionqu/.nvm/versions/node/v20.9.0/bin/:/opt/homebrew/opt/coreutils/libexec/gnubin:/Applications/Emacs.app/Contents/MacOS/bin/:/Users/lionqu/Library/Python/2.7/bin:/Users/lionqu/.pyenv/shims:/Users/lionqu/.emacs.d/bin:/usr/local/sbin:/Applications/iTerm.app/Contents/Resources/utilities:/Users/lionqu/Library/Python/3.8/bin:~/.cargo/bin:/Users/lionqu/.antigen/bundles/robbyrussell/oh-my-zsh/lib:/Users/lionqu/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/git:/Users/lionqu/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/ruby:/Users/lionqu/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/pip:/Users/lionqu/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/lein:/Users/lionqu/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/command-not-found:/Users/lionqu/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/gulp:/Users/lionqu/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/node:/Users/lionqu/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/nvm:/Users/lionqu/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/bower:/opt/homebrew/bin:/Users/lionqu/Library/Python/3.8/bin")
(setq exec-path (split-string (getenv "PATH") path-separator))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
