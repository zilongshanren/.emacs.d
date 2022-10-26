;;; init-funcs.el -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2021-2022 zilongshanren

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
;; Define functions.
;;

;;; Code:

(require 'cl-lib)

(require 'init-const)
(require 'init-custom)


(unless (fboundp 'caadr)
  (defalias 'caadr #'cl-caadr))


;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-carrage-returns ()
  "Delete `^M' characters in the buffer.
Same as `replace-string C-q C-m RET RET'."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (if-let ((filename (if (equal major-mode 'dired-mode)
                         default-directory
                       (buffer-file-name))))
      (progn
        (kill-new filename)
        (message "Copied '%s'" filename))
    (warn "Current buffer is not attached to a file!")))

(defun copy-buffer-name ()
  "Copy name of the current buffer."
  (interactive)
  (kill-new (buffer-name)))

;; Browse URL
(defun zilongshanren-webkit-browse-url (url &optional pop-buffer new-session)
  "Browse url with webkit and switch or pop to the buffer.
POP-BUFFER specifies whether to pop to the buffer.
NEW-SESSION specifies whether to create a new xwidget-webkit session."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (when (and (featurep 'xwidget-internal)
             (fboundp 'xwidget-buffer)
             (fboundp 'xwidget-webkit-current-session))
    (xwidget-webkit-browse-url url new-session)
    (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
      (when (buffer-live-p buf)
        (and (eq buf (current-buffer)) (quit-window))
        (if pop-buffer
            (pop-to-buffer buf)
          (switch-to-buffer buf))))))




;; Mode line
(defun mode-line-height ()
  "Get the height of the mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))


;; Open custom file
(defun open-custom-file()
  "Open or create `custom-file'."
  (interactive)
  (unless (file-exists-p custom-file)
    (if (file-exists-p zilongshanren-custom-example-file)
        (copy-file zilongshanren-custom-example-file custom-file)
      (user-error "The file `%s' doesn't exist" zilongshanren-custom-example-file)))
  (find-file custom-file)
  (find-file-other-window zilongshanren-custom-post-file))

;; Misc
(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (let ((temp-dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory temp-dir)
      (byte-recompile-directory temp-dir 0 t))))

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and zilongshanren-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)))

(defun zilongshanren-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

Save to `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
              nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))


;; Pakcage repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package archives (ELPA).

REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern (completing-read "Select package archives: "
                             (mapcar #'car zilongshanren-package-archives-alist)))))
  ;; Set option
  (zilongshanren-set-variable 'zilongshanren-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  ;; (message "Set package archives to `%s'" archives)
  )
(defalias 'zilongshanren-set-package-archives #'set-package-archives)


;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen.
This issue has been addressed in 28."
  (and sys/mac-cocoa-p
       emacs/>=26p
       (not emacs/>=28p)
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))



;; Fonts
(defun zilongshanren-install-fonts ()
  "Install necessary fonts."
  (interactive)

  (let* ((url-format "https://raw.githubusercontent.com/domtronn/all-the-icons.el/master/fonts/%s")
         (url (concat zilongshanren-homepage "/files/6135060/symbola.zip"))
         (font-dest (cond
                     ;; Default Linux install directories
                     ((member system-type '(gnu gnu/linux gnu/kfreebsd))
                      (concat (or (getenv "XDG_DATA_HOME")
                                  (concat (getenv "HOME") "/.local/share"))
                              "/fonts/"))
                     ;; Default MacOS install directory
                     ((eq system-type 'darwin)
                      (concat (getenv "HOME") "/Library/Fonts/"))))
         (known-dest? (stringp font-dest))
         (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))

    (unless (file-directory-p font-dest) (mkdir font-dest t))

    ;; Download `all-the-fonts'
    (when (bound-and-true-p all-the-icons-font-names)
      (mapc (lambda (font)
              (url-copy-file (format url-format font) (expand-file-name font font-dest) t))
            all-the-icons-font-names))

    ;; Download `Symbola'
    ;; See https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.zip
    (let* ((temp-file (make-temp-file "symbola-" nil ".zip"))
           (temp-dir (concat (file-name-directory temp-file) "/symbola/"))
           (unzip-script (cond ((executable-find "unzip")
                                (format "mkdir -p %s && unzip -qq %s -d %s"
                                        temp-dir temp-file temp-dir))
                               ((executable-find "powershell")
                                (format "powershell -noprofile -noninteractive \
  -nologo -ex bypass Expand-Archive -path '%s' -dest '%s'" temp-file temp-dir))
                               (t (user-error "Unable to extract '%s' to '%s'! \
  Please check unzip, powershell or extract manually." temp-file temp-dir)))))
      (url-copy-file url temp-file t)
      (when (file-exists-p temp-file)
        (shell-command-to-string unzip-script)
        (let* ((font-name "Symbola.otf")
               (temp-font (expand-file-name font-name temp-dir)))
          (if (file-exists-p temp-font)
              (copy-file temp-font (expand-file-name font-name font-dest) t)
            (message "Failed to download `Symbola'!")))))

    (when known-dest?
      (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
      (shell-command-to-string (format "fc-cache -f -v")))

    (message "Successfully %s `all-the-icons' and `Symbola' fonts to `%s'!"
             (if known-dest? "installed" "downloaded")
             font-dest)))


(defun zilongshanren/insert-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (zilongshanren/retrieve-chrome-current-tab-url)))

(defun zilongshanren/retrieve-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
		 (concat
		  "set frontmostApplication to path to frontmost application\n"
		  "tell application \"Google Chrome\"\n"
		  "	set theUrl to get URL of active tab of first window\n"
		  "	set theResult to (get theUrl) \n"
		  "end tell\n"
		  "activate application (frontmostApplication as text)\n"
		  "set links to {}\n"
		  "copy theResult to the end of links\n"
		  "return links as string\n"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))


(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-buffer)
        (message "Indent buffer.")))))

;;http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun zilongshanren/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))


(defun zilongshanren/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun zilongshanren/yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (deactivate-mark)
  (call-interactively 'occur))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

(defun dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files
                 t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: "
                                current-prefix-arg files)
      files)))
  (let (list-switch)
    (start-process
     cmd nil shell-file-name
     shell-command-switch
     (format
      "nohup 1>/dev/null 2>/dev/null %s \"%s\""
      (if (and (> (length file-list) 1)
               (setq list-switch
                     (cadr (assoc cmd dired-filelist-cmd))))
          (format "%s %s" cmd list-switch)
        cmd)
      (mapconcat #'expand-file-name file-list "\" \"")))))

(defun dired-open-term ()
  "Open an `ansi-term' that corresponds to current directory."
  (interactive)
  (let* ((current-dir (dired-current-directory))
         (buffer (if (get-buffer "*zshell*")
                     (switch-to-buffer "*zshell*")
                   (ansi-term "/bin/zsh" "zshell")))
         (proc (get-buffer-process buffer)))
    (term-send-string
     proc
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))

(defun dired-copy-file-here (file)
  (interactive "fCopy file: ")
  (copy-file file default-directory))

(defun my-dired-find-file ()
  "Open buffer in another window"
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (if (car (file-attributes filename))
        (dired-find-alternate-file)
      (dired-find-file-other-window))))

(defun zilongshanren/dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

(defun zilongshanren/dired-up-directory()
  "goto up directory and resue buffer"
  (interactive)
  (find-alternate-file ".."))

(defun zilongshanren/insert-space-after-point ()
  (interactive)
  (save-excursion (insert " ")))


(defmacro dakra-define-up/downcase-dwim (case)
  (let ((func (intern (concat "dakra-" case "-dwim")))
        (doc (format "Like `%s-dwim' but %s from beginning when no region is active." case case))
        (case-region (intern (concat case "-region")))
        (case-word (intern (concat case "-word"))))
    `(defun ,func (arg)
       ,doc
       (interactive "*p")
       (save-excursion
         (if (use-region-p)
             (,case-region (region-beginning) (region-end))
           (beginning-of-thing 'symbol)
           (,case-word arg))))))

(dakra-define-up/downcase-dwim "upcase")
(dakra-define-up/downcase-dwim "downcase")
(dakra-define-up/downcase-dwim "capitalize")


(defun zilongshanren/consult-line (consult-line-function &rest rest)
  "Advising function around `CONSULT-LINE-FUNCTION'.

When there's an active region, use that as the first parameter
for `CONSULT-LINE-FUNCTION'.  Otherwise, use the current word as
the first parameter.  This function handles the `REST' of the
parameters."
  (interactive)
  (if (use-region-p)
      (apply consult-line-function
        (buffer-substring (region-beginning) (region-end)) rest)
      (apply consult-line-function
             rest)))



(defun +vertico/embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (pcase-let ((`(,type . ,candidates)
               (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (pcase type
      ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                       (embark-export)))
      ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
               (embark-export)))
      ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                           (embark-export)))
      (x (user-error "embark category %S doesn't support writable export" x)))))

(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))



(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window.
If `spacemacs-layouts-restrict-spc-tab' is `t' then this only switches between
the current layouts buffers."
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (if (bound-and-true-p spacemacs-layouts-restrict-spc-tab)
          (let ((buffer-list (persp-buffer-list))
                (my-buffer (window-buffer window)))
            ;; find buffer of the same persp in window
            (seq-find (lambda (it) ;; predicate
                        (and (not (eq (car it) my-buffer))
                             (member (car it) buffer-list)))
                      (window-prev-buffers)
                      ;; default if found none
                      (list nil nil nil)))
        (or (cl-find (window-buffer window) (window-prev-buffers)
                     :key #'car :test-not #'eq)
            (list (other-buffer) nil nil)))
    (if (not buf)
        (message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))

(defun open-my-init-file()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory )))

(defun my-toggle-line-numbber ()
  (interactive)
  (if global-display-line-numbers-mode
      (global-display-line-numbers-mode -1)
    (global-display-line-numbers-mode 1)))

;; (defun doom/escape (&optional interactive)
;;   "Run `doom-escape-hook'."
;;   (interactive (list 'interactive))
;;   (cond ((minibuffer-window-active-p (minibuffer-window))
;;          ;; quit the minibuffer if open.
;;          (when interactive
;;            (setq this-command 'abort-recursive-edit))
;;          (abort-recursive-edit))
;;         ;; don't abort macros
;;         ((or defining-kbd-macro executing-kbd-macro) nil)
;;         ;; Back to the default
;;         ((unwind-protect (keyboard-quit)
;;            (when interactive
;;              (setq this-command 'keyboard-quit))))))

;; (global-set-key [remap keyboard-quit] #'doom/escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape))

(defun zilongshanren/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

;; https://emacs-china.org/t/advice/7566
;;;###autoload
(defun function-advices (function)
  "Return FUNCTION's advices."
  (let ((flist (indirect-function function)) advices)
    (while (advice--p flist)
      (setq advices `(,@advices ,(advice--car flist)))
      (setq flist (advice--cdr flist)))
    advices))

;; Modified from the original function written by @xuchunyang (https://emacs-china.org/t/advice/7566/)
(define-advice describe-function-1 (:after (function) advice-remove-button)
  "Add a button to remove advice."
  (when (get-buffer "*Help*")
    (with-current-buffer "*Help*"
      (save-excursion
        (goto-char (point-min))
        (let ((ad-list (function-advices function)))
          (while (re-search-forward "^\\(?:This function has \\)?:[-a-z]+ advice: \\(.+\\)\\.?$" nil t)
            (let* ((name (string-trim (match-string 1) "[‘'`]" "[’']"))
                   (symbol (intern-soft name))
                   (advice (or symbol (car ad-list))))
              (when advice
                (when symbol
                  (cl-assert (eq symbol (car ad-list))))
                (let ((inhibit-read-only t))
                  (insert " » ")
                  (insert-text-button
                   "Remove"
                   'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
                   'help-echo (format "%s" advice)
                   'action
                   ;; In case lexical-binding is off
                   `(lambda (_)
                      (when (yes-or-no-p (format "Remove %s ? " ',advice))
                        (message "Removing %s of advice from %s" ',function ',advice)
                        (advice-remove ',function ',advice)
                        (revert-buffer nil t)))
                   'follow-link t))))
            (setq ad-list (cdr ad-list))))))))


;; http://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type
;;;###autoload
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

;;http://emacsredux.com/blog/2013/03/26/smarter-open-line/
;;;###autoload
(defun zilongshanren/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))


;;;###autoload
(defun zilongshanren/yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

;;;###autoload
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (deactivate-mark)
  (call-interactively 'occur))

;;;###autoload
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

;;;###autoload
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

;;;###autoload
(defun dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files
                 t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: "
                                current-prefix-arg files)
      files)))
  (let (list-switch)
    (start-process
     cmd nil shell-file-name
     shell-command-switch
     (format
      "nohup 1>/dev/null 2>/dev/null %s \"%s\""
      (if (and (> (length file-list) 1)
               (setq list-switch
                     (cadr (assoc cmd dired-filelist-cmd))))
          (format "%s %s" cmd list-switch)
        cmd)
      (mapconcat #'expand-file-name file-list "\" \"")))))

;;;###autoload
(defun dired-open-term ()
  "Open an `ansi-term' that corresponds to current directory."
  (interactive)
  (let* ((current-dir (dired-current-directory))
         (buffer (if (get-buffer "*zshell*")
                     (switch-to-buffer "*zshell*")
                   (ansi-term "/bin/zsh" "zshell")))
         (proc (get-buffer-process buffer)))
    (term-send-string
     proc
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))

;;;###autoload
(defun dired-copy-file-here (file)
  (interactive "fCopy file: ")
  (copy-file file default-directory))

;;;###autoload
(defun my-dired-find-file ()
  "Open buffer in another window"
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (if (car (file-attributes filename))
        (dired-find-alternate-file)
      (dired-find-file-other-window))))

;;;###autoload
(defun zilongshanren/dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)

            (call-interactively command))
          (dired-get-marked-files))))

;;;###autoload
(defun zilongshanren/dired-up-directory()
  "goto up directory and resue buffer"
  (interactive)
  (find-alternate-file ".."))

;;;###autoload
(defun zilongshanren/insert-space-after-point ()
  (interactive)
  (save-excursion (insert " ")))

;;;###autoload
(defun ora-ediff-files ()
      (interactive)
      (let ((files (dired-get-marked-files))
            (wnd (current-window-configuration)))
        (if (<= (length files) 2)
            (let ((file1 (car files))
                  (file2 (if (cdr files)
                             (cadr files)
                           (read-file-name
                            "file: "
                            (dired-dwim-target-directory)))))
              (if (file-newer-than-file-p file1 file2)
                  (ediff-files file2 file1)
                (ediff-files file1 file2))
              (add-hook 'ediff-after-quit-hook-internal
                        (lambda ()
                          (setq ediff-after-quit-hook-internal nil)
                          (set-window-configuration wnd))))
          (error "no more than 2 files should be marked"))))

;;;###autoload
(defun ffap-hexl-mode ()
  (interactive)
  (let ((ffap-file-finder 'hexl-find-file))
    (call-interactively 'ffap)))

;;;###autoload
(defun browse-hugo-maybe ()
  (interactive)
  (let ((hugo-service-name "Hugo Server")
        (hugo-service-port "1313"))
    (if (prodigy-service-started-p (prodigy-find-service hugo-service-name))
        (progn
          (message "Hugo detected, launching browser...")
          (browse-url (concat "http://localhost:" hugo-service-port))))))

;;;###autoload
(defun zilongshanren/highlight-dwim ()
  (interactive)
  (if (use-region-p)
      (progn
        (highlight-frame-toggle)
        (deactivate-mark))
    (symbol-overlay-put)))

;;;###autoload
(defun my/search-project-for-symbol-at-point ()
  (interactive)
  (if (use-region-p)
      (progn
        (consult-ripgrep (project-root (project-current))
                         (buffer-substring (region-beginning) (region-end))))))

;;;###autoload
(defun zilongshanren/clearn-highlight ()
  (interactive)
  (clear-highlight-frame)
  (symbol-overlay-remove-all))

;;;###autoload
(defun zilongshanren/my-mc-mark-next-like-this ()
  (interactive)
  (if (region-active-p)
      (mc/mark-next-like-this 1)
    (er/expand-region 1)))


;;;###autoload
(defun wrap-sexp-with-new-round-parens ()
  (interactive)
  (insert "()")
  (backward-char)
  (sp-forward-slurp-sexp))

;;;###autoload
(defun evil-paste-after-from-0 ()
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after)))

;;;###autoload
(defun my-erc-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (zilongshanren/growl-notification
     (concat "ERC: : " (buffer-name (current-buffer)))
     message
     t
     )))

;; "http://xuchunyang.me/Opening-iTerm-From-an-Emacs-Buffer/"
;;;###autoload
(defun zilongshanren/iterm-shell-command (command &optional prefix)
  "cd to `default-directory' then run COMMAND in iTerm.
With PREFIX, cd to project root."
  (interactive (list (read-shell-command
                      "iTerm Shell Command: ")
                     current-prefix-arg))
  (let* ((dir (if prefix (zilongshanren/git-project-root)
                default-directory))
         ;; if COMMAND is empty, just change directory
         (cmd (format "cd %s ;%s" dir command)))
    (do-applescript
     (format
      "
  tell application \"iTerm2\"
       activate
       set _session to current session of current window
       tell _session
            set command to get the clipboard
            write text \"%s\"
       end tell
  end tell
  " cmd))))

;;;###autoload
(defun zilongshanren/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

;;;###autoload
(defun zilongshanren/git-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (locate-dominating-file directory ".git")))

;; insert date and time
;;;###autoload
(defun zilongshanren/now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%H:%M:%S" (current-time))))

;;;###autoload
(defun zilongshanren/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))

;; https://github.com/syohex/emacs-browser-refresh/blob/master/browser-refresh.el
;;;###autoload
(defun zilongshanren/browser-refresh--chrome-applescript ()
  (interactive)
  (do-applescript
   (format
    "
  tell application \"Google Chrome\"
    set winref to a reference to (first window whose title does not start with \"Developer Tools - \")
    set winref's index to 1
    reload active tab of winref
  end tell
" )))


(defun my-open-current-directory ()
  (interactive)
  (consult-directory-externally default-directory))


(defun consult-snv-unlock (file)
  "unlock svn file lock forcelly"
  (interactive "fOpen file: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\" (format "svn unlock --force %s" (expand-file-name file))) 'gbk))))

(defun consult-directory-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\" (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk))
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil
                  (file-name-directory (expand-file-name file)))))

;; (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\" (format "explorer.exe %s" (file-name-directory "F:\\workspace\\Configuration\\Mobile\\Live\\全版本_3.0\\Puffer"))) 'gbk))
;; (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\" (format "explorer.exe %s" (file-name-directory file))) 'gbk))


(defun zilongshanren/directory-parent (directory)
  (let ((parent (file-name-directory (directory-file-name directory))))
    (if (not (equal directory parent))
        parent)))



;; Screenshot
(defun zilongshanren//insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))

(defun zilongshanren/capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (progn
    (setq final-image-full-path (concat basename ".png"))
    (call-process "screencapture" nil nil nil "-s" final-image-full-path)
    (if (executable-find "convert")
        (progn
          (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
          (shell-command-to-string resize-command-str)))
    (zilongshanren//insert-org-or-md-img-link "./" (concat basename ".png")))
  (insert "\n"))

(defun zilongshanren/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(defun zilongshanren/org-archive-cancel-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file))

;; "https://github.com/vhallac/.emacs.d/blob/master/config/customize-org-agenda.el"
(defun zilongshanren/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; VH: I changed this line from
      ;; (if (bh/is-project-p)
      (if (and (eq (point) (bh/find-project-task))
               (bh/is-project-p))
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun zilongshanren/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "typescript" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))



(defun zilong/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun zilong/filter-by-tags ()
  (let ((head-tags (org-get-tags-at)))
    (member current-tag head-tags)))

(defun zilong/org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                         "LIFE" "PROJECT" "OTHER"))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart 86400)))
         h m file item prompt donesomething)
    (while (setq file (pop files))
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (current-tag include-tags)
          (org-clock-sum tstart tend 'zilong/filter-by-tags)
          (setcdr (assoc current-tag tags-time-alist)
                  (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)
        (setq donesomething t)
        (setq h (/ (cdr item) 60)
              m (- (cdr item) (* 60 h)))
        (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
      (insert output-string))
    output-string))

;;;###autoload
(defun zilongshanren/hotspots ()
  (interactive)
  (require 'consult)
  (setq-local source '(("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                       ("RSS" . elfeed)
                       ("Blog" . browse-hugo-maybe)
                       ("Search" . (lambda () (call-interactively #'engine/search-google)))
                       ("Random Todo" . org-random-entry)
                       ("lsp bridge" . my/enable-lsp-bridge)
                       ("string edit" . separedit)
                       ("Org Roam" . org-roam-find-file)
                       ("Github" . (lambda() (helm-github-stars)))
                       ("Prodigy" . (lambda() (prodigy)))

                       ;;todo (calc-eval "(1+1)*3")
                       ;; ("Calculator" . (lambda () (helm-calcul-expression)))
                       ("Run current flie" . (lambda () (zilongshanren/run-current-file)))
                       ("Agenda" . (lambda () (org-agenda "" "a")))
                       ("sicp" . (lambda() (browse-url "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start")))))
  (let* ((result (consult--read (mapcar 'car source) :prompt "zilong's hotpot ")))
    (when result
      (funcall (cdr (assoc result source))))))

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(defun timestamp-to-date (seconds)
  (interactive "n")
  (message (kill-new (format-time-string "%Y-%m-%d-%H-%M-%S" (seconds-to-time seconds)))))


(defun date-to-timestamp (date)
  (interactive (list (read-from-minibuffer "" (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))
  (message (kill-new (format-time-string "%s" (seconds-to-time (org-time-string-to-time date))))))

(defun switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (startup--get-buffer-create-scratch)))

(defun vc-print-log-internal (backend files working-revision
                                      &optional is-start-revision limit)
  "For specified BACKEND and FILES, show the VC log.
Leave point at WORKING-REVISION, if it is non-nil.
If IS-START-REVISION is non-nil, start the log from WORKING-REVISION
\(not all backends support this); i.e., show only WORKING-REVISION and
earlier revisions.  Show up to LIMIT entries (non-nil means unlimited)."
  ;; As of 2013/04 the only thing that passes IS-START-REVISION non-nil
  ;; is vc-annotate-show-log-revision-at-line, which sets LIMIT = 1.

  ;; Don't switch to the output buffer before running the command,
  ;; so that any buffer-local settings in the vc-controlled
  ;; buffer can be accessed by the command.
  (let* ((dir-present (cl-some #'file-directory-p files))
         (shortlog (not (null (memq (if dir-present 'directory 'file)
                                    vc-log-short-style))))
         (buffer-name "*vc-change-log*")
         (type (if shortlog 'short 'long))
         (coding-system-for-read 'gbk)) ;; 读取的是GBK编码
    (vc-log-internal-common
     backend buffer-name files type
     (lambda (bk buf _type-arg files-arg)
       (vc-call-backend bk 'print-log files-arg buf shortlog
                        (when is-start-revision working-revision) limit))
     (lambda (_bk _files-arg ret)
       (vc-print-log-setup-buttons working-revision
                                   is-start-revision limit ret))
     ;; When it's nil, point really shouldn't move (bug#15322).
     (when working-revision
       (lambda (bk)
         (vc-call-backend bk 'show-log-entry working-revision)))
     (lambda (_ignore-auto _noconfirm)
       (vc-print-log-internal backend files working-revision
                              is-start-revision limit)))))

(defun terminal-notifier (title msg)
  (call-process "terminal-notifier" nil 0 nil "-group" "Emacs" "-title" title "-activate" "org.gnu.Emacs" "-message" msg))

(defun disable-curly-bracket-electric-pair ()
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?{) t (,electric-pair-inhibit-predicate c)))))

(defun my/project-try-local (dir)
  "Determine if DIR is a non-Git project."
  (catch 'ret
    (let ((pr-flags '((".project")
                      )))
      (dolist (current-level pr-flags)
        (dolist (f current-level)
          (when-let ((root (locate-dominating-file dir f)))
            (throw 'ret (cons 'local root))))))))

(defun my/project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -H -t f -0 . %s -E .git -E \"*.meta\" -E StreamingAssets" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan #'my/project-files-in-directory
          (or dirs (list (project-root project)))))

(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun my/add-dot-project ()
  (interactive)
  (let* ((root-dir (read-directory-name "Root: "))
         (f (expand-file-name ".project" root-dir)))
    (message "Create %s..." f)
    (make-empty-file f)))

(defun my/project-info ()
  (interactive)
  (message "%s" (project-current t)))

(defun my/makefile-targets (dir)
  "Find Makefile targets in dir. https://stackoverflow.com/a/58316463/2163429"
  (let* ((default-directory dir))
	(with-temp-buffer
	  (insert (shell-command-to-string "make -qp"))
	  (goto-char (point-min))
	  (let ((targets '()))
		(while (re-search-forward "^\\([a-zA-Z0-9][^$#\\/\\t=]*\\):[^=|$]" nil t)
		  (let ((target (match-string 1)))
			(unless (member target '("Makefile" "make" "makefile" "GNUmakefile"))
			  (push target targets))))
		(sort targets 'string-lessp)))))

(defun my/project-run-makefile-target ()
  (interactive)
  (let* ((pr (project-current t))
		 (default-directory (project-root pr))
		 (target (completing-read "Target: " (my/makefile-targets default-directory)))
         (buf-name "*Async Makefile Target*"))
    (when-let (b (get-buffer buf-name))
      (kill-buffer b))
	(async-shell-command (concat "make " (shell-quote-argument target)) buf-name)))

(defun my/project-citre ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (citre-create-tags-file)
    (add-dir-local-variable 'prog-mode 'eval '(citre-mode))))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

 (defun my/rename-current-buffer-file ()
    "Renames current buffer and file it is visiting."
    (interactive)
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (error "Buffer '%s' is not visiting a file!" name)
        (let ((new-name (read-file-name "New name: " filename)))
          (if (get-buffer new-name)
              (error "A buffer named '%s' already exists!" new-name)
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)
            (message "File '%s' successfully renamed to '%s'"
                     name (file-name-nondirectory new-name)))))))

(defun my/delete-file-and-buffer ()
  "Kill the current buffer and delete the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

  (defun my/exec-shell-on-buffer (shell-command-text)
    (interactive "MShell command: ")
    (shell-command (format "%s %s" shell-command-text (shell-quote-argument buffer-file-name))))

(defun my/imenu ()
  (interactive)
  (if (eq major-mode #'org-mode)
      (call-interactively #'consult-org-heading)
    (call-interactively #'consult-imenu)))

(defun av/auto-indent-method ()
  "Automatically indent a method by adding two newlines.
Puts point in the middle line as well as indent it by correct amount."
  (interactive)
  (newline-and-indent)
  (newline-and-indent)
  (forward-line -1)
  (cond ((eq major-mode 'rust-mode)
         (rust-mode-indent-line))
        ((eq major-mode 'dart-mode)
         (dart-indent-simple))
        (t (c-indent-line-or-region))))

(defun av/auto-indent-method-maybe ()
  "Check if point is at a closing brace then auto indent."
  (interactive)
  (let ((char-at-point (char-after (point))))
    (if (char-equal ?} char-at-point)
        (av/auto-indent-method)
      (newline-and-indent))))

(defun zilongshanren/run-current-file ()
  (interactive)
  (quickrun-shell))

(defun my-goto-next-error ()
  (interactive)
  (if (bound-and-true-p flycheck-mode)
      (flycheck-next-error)
    (flymake-goto-next-error)))

(defun my-goto-previous-error ()
  (interactive)
  (if (bound-and-true-p flycheck-mode)
      (flycheck-previous-error)
    (flymake-goto-prev-error)))

(defun my-list-errors ()
  (interactive)
  (if (bound-and-true-p flycheck-mode)
      (flycheck-list-errors)
    (flymake-show-buffer-diagnostics)))

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))


(defun json-to-single-line (beg end)
  "Collapse prettified json in region between BEG and END to a single line"
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "[[:space:]\n]+" nil t)
            (replace-match " "))))
    (print "This function operates on a region")))

(defun my-project-imenu()
      (interactive)
      (if (bound-and-true-p eglot--managed-mode)
          (call-interactively 'consult-eglot-symbols) ;; 第三方包consult-eglot
        (call-interactively 'consult-imenu-multi))) ;; consult-imenu.el里有

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
