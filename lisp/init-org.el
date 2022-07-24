;;; init-org.el -*- lexical-binding: t no-byte-compile: t -*-

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




(use-package org-pomodoro
  :ensure t
  :commands org-pomodoro
  :after org)


(use-package org-super-agenda
  :after org
  :init
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (define-key org-super-agenda-header-map (kbd "q") 'org-agenda-quit)
  (setq org-super-agenda-groups
        '((:name "Important"
                 :priority "A")
          (:name "Quick Picks"
                 :effort< "0:30")
          (:name "Next Items"
                 :tag ("NEXT" "outbox"))
          (:priority<= "B"
                       :scheduled future)))
  (add-hook 'org-agenda-mode-hook
            'org-super-agenda-mode))

(use-package evil-org
  :ensure t
  :hook (org-mode . evil-org-mode)
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(defun org-apperance-evil-hack ()
  (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
  (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))

(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook 'org-apperance-evil-hack))

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t))

(use-package org
  :ensure t
  :pin gnu)

(with-eval-after-load 'org
  (progn
    ;; If you intend to use org, it is recommended you change this!
    (setq org-directory "~/org-notes/")

    (setq org-startup-indented t
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-startup-with-inline-images t
          org-image-actual-width '(300))

    (setq org-ellipsis "⤵")


    (setq org-emphasis-alist
          '(("*" my-org-emphasis-bold)
            ("/" my-org-emphasis-italic)
            ("_" underline)
            ("=" org-verbatim verbatim)
            ("~" org-code verbatim)
            ("+" (:strike-through t))))

    (defface my-org-emphasis-bold
      '((default :inherit bold)
        (((class color) (min-colors 88) (background light))
         :foreground "#a60000")
        (((class color) (min-colors 88) (background dark))
         :foreground "#ff8059"))
      "My bold emphasis for Org.")

    (defface my-org-emphasis-italic
      '((default :inherit italic)
        (((class color) (min-colors 55) (background light))
         :foreground "#972500")
        (((class color) (min-colors 55) (background dark))
         :foreground "#ef8b50"))
      "My italic emphasis for Org.")


    (defvar org-agenda-dir ""
      "gtd org files location")

    (defvar deft-dir ""
      "deft org files locaiton")

    (setq org-agenda-dir "~/org-notes/")
    (setq deft-dir  "~/org-notes/")


    ;; https://emacs-china.org/t/ox-hugo-auto-fill-mode-markdown/9547/4
    (defadvice org-hugo-paragraph (before org-hugo-paragraph-advice
                                          (paragraph contents info) activate)
      "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to hugo markdown."
      (let* ((origin-contents (ad-get-arg 1))
             (fix-regexp "[[:multibyte:]]")
             (fixed-contents
              (replace-regexp-in-string
               (concat
                "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
        (ad-set-arg 1 fixed-contents)))


    (require 'org-tempo)
    ;; Allow multiple line Org emphasis markup.
    ;; http://emacs.stackexchange.com/a/13828/115
    (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
    ;; Below is needed to apply the modified `org-emphasis-regexp-components'
    ;; settings from above.
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

    (setq org-agenda-log-mode-items '(clock closed state))


    (defun zilong/org-return (&optional indent)
      "Goto next table row or insert a newline.
Calls `org-table-next-row' or `newline', depending on context.
When optional INDENT argument is non-nil, call
`newline-and-indent' instead of `newline'.
When `org-return-follows-link' is non-nil and point is on
a timestamp or a link, call `org-open-at-point'.  However, it
will not happen if point is in a table or on a \"dead\"
object (e.g., within a comment).  In these case, you need to use
`org-open-at-point' directly."
      (interactive)
      (let ((context (if org-return-follows-link (org-element-context)
                       (org-element-at-point))))
        (cond
         ;; In a table, call `org-table-next-row'.  However, before first
         ;; column or after last one, split the table.
         ((or (and (eq 'table (org-element-type context))
                   (not (eq 'table.el (org-element-property :type context)))
                   (>= (point) (org-element-property :contents-begin context))
                   (< (point) (org-element-property :contents-end context)))
              (org-element-lineage context '(table-row table-cell) t))
          (if (or (looking-at-p "[ \t]*$")
                  (save-excursion (skip-chars-backward " \t") (bolp)))
              (insert "\n")
            (org-table-justify-field-maybe)
            (call-interactively #'org-table-next-row)))
         ;; On a link or a timestamp, call `org-open-at-point' if
         ;; `org-return-follows-link' allows it.  Tolerate fuzzy
         ;; locations, e.g., in a comment, as `org-open-at-point'.
         ((and org-return-follows-link
               (or (and (eq 'link (org-element-type context))
                        ;; Ensure point is not on the white spaces after
                        ;; the link.
                        (let ((origin (point)))
                          (org-with-point-at (org-element-property :end context)
                            (skip-chars-backward " \t")
                            (> (point) origin))))
                   (org-in-regexp org-ts-regexp-both nil t)
                   (org-in-regexp org-tsr-regexp-both nil t)
                   (org-in-regexp org-any-link-re nil t)))
          (call-interactively #'org-open-at-point))
         ;; Insert newline in heading, but preserve tags.
         ((and (not (bolp))
               (let ((case-fold-search nil))
                 (org-match-line org-complex-heading-regexp)))
          ;; At headline.  Split line.  However, if point is on keyword,
          ;; priority cookie or tags, do not break any of them: add
          ;; a newline after the headline instead.
          (let ((tags-column (and (match-beginning 5)
                                  (save-excursion (goto-char (match-beginning 5))
                                                  (current-column))))
                (string
                 (when (and (match-end 4) (org-point-in-group (point) 4))
                   (delete-and-extract-region (point) (match-end 4)))))
            ;; Adjust tag alignment.
            (cond
             ((not (and tags-column string)))
             (org-auto-align-tags (org-align-tags))
             (t (org--align-tags-here tags-column))) ;preserve tags column
            (end-of-line)
            (org-show-entry)
            (if indent (newline-and-indent) (newline))
            (when string (save-excursion (insert (org-trim string))))))
         ;; In a list, make sure indenting keeps trailing text within.
         ((and indent
               (not (eolp))
               (org-element-lineage context '(item)))
          (let ((trailing-data
                 (delete-and-extract-region (point) (line-end-position))))
            (newline-and-indent)
            (save-excursion (insert trailing-data))))
         ((and (eolp) (org-at-item-p))
          (end-of-visible-line)
          (org-insert-item (org-at-item-checkbox-p)))
         (t
          ;; Do not auto-fill when point is in an Org property drawer.
          (let ((auto-fill-function (and (not (org-at-property-p))
                                         auto-fill-function)))
            (if indent
                (newline-and-indent)
              (newline)))))))


    (define-key org-mode-map (kbd "RET")
      'zilong/org-return)

    (evil-define-key 'normal org-mode-map
      "+" #'org-cycle-list-bullet)


    (setq org-complete-tags-always-offer-all-agenda-tags t)

    (require 'org-compat)
    (require 'org)
    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)

    ;; 调整orghabit 的显示长度
    (setq org-habit-graph-column 60)

    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
          '((nil :maxlevel . 4)
            (org-agenda-files :maxlevel . 4)))
    ;; config stuck project
    (setq org-stuck-projects
          '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

    (setq org-agenda-inhibit-startup t) ;; ~50x speedup
    (setq org-agenda-span 'day)
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    (setq org-agenda-window-setup 'current-window)
    (setq org-log-done t)

    ;; 加密文章
    ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
    ;; org-mode 設定
    (require 'org-crypt)

    ;; 當被加密的部份要存入硬碟時，自動加密回去
    (org-crypt-use-before-save-magic)

    ;; 設定要加密的 tag 標籤為 secret
    (setq org-crypt-tag-matcher "secret")

    ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
    ;; (但是子項目還是會被加密喔)
    (setq org-tags-exclude-from-inheritance (quote ("secret")))

    (setq org-columns-default-format "%60ITEM(Task) %6Effort(Estim){:}")

    ;; 在agenda里面显示efforts
    (require 'cl-lib)

	(defun my/org-agenda-calculate-efforts (limit)
      "Sum the efforts of scheduled entries up to LIMIT in the
        agenda buffer."
      (let (total)
        (save-excursion
          (while (< (point) limit)
            (when (member (org-get-at-bol 'type) '("scheduled" "past-scheduled"))
              (push (org-entry-get (org-get-at-bol 'org-hd-marker) "Effort") total))
            (forward-line)))
        (org-duration-from-minutes
         (cl-reduce #'+
                    (mapcar #'org-duration-to-minutes
                            (cl-remove-if-not 'identity total))))))

	(defun my/org-agenda-insert-efforts ()
      "Insert the efforts for each day inside the agenda buffer."
      (save-excursion
        (let (pos)
          (while (setq pos (text-property-any
                            (point) (point-max) 'org-agenda-date-header t))
            (goto-char pos)
            (end-of-line)
            (insert-and-inherit (concat " ("
                                        (my/org-agenda-calculate-efforts
                                         (next-single-property-change (point) 'day))
                                        ")"))
            (forward-line)))))

    (add-hook 'org-agenda-finalize-hook 'my/org-agenda-insert-efforts)

    ;; 用於加密的 GPG 金鑰
    ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
    (setq org-crypt-key nil)

    (require 'cal-china)
    ;; diary for chinese birthday
    ;; https://emacs-china.org/t/topic/2119/14
    (defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
      (if year
          (let* ((d-date (diary-make-date lunar-month lunar-day year))
                 (a-date (calendar-absolute-from-gregorian d-date))
                 (c-date (calendar-chinese-from-absolute a-date))
                 (date a-date)
                 (cycle (car c-date))
                 (yy (cadr c-date))
                 (y (+ (* 100 cycle) yy)))
            (diary-chinese-anniversary lunar-month lunar-day y mark))
        (diary-chinese-anniversary lunar-month lunar-day year mark)))


    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq org-agenda-start-day "+0d")
    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line

    (setq org-tags-match-list-sublevels nil)

    (add-hook 'org-mode-hook (lambda ()
                               ;; keybinding for editing source code blocks
                               (when (featurep 'company)
                                 (company-mode -1))
                               ;; keybinding for inserting code blocks
                               (local-set-key (kbd "C-c i s")
                                              'zilongshanren/org-insert-src-block)))
    (require 'ox-publish)
    (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    ;; {{ export org-mode in Chinese into PDF
    ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
    ;; and you need install texlive-xetex on different platforms
    ;; To install texlive-xetex:
    ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
    ;; }}
    (setq org-latex-default-class "ctexart")
    (setq org-latex-pdf-process
          '(
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "rm -fr %b.out %b.log %b.tex auto"))

    (setq org-latex-listings t)

    (defun org-random-entry (&optional arg)
      "Select and goto a random todo item from the global agenda"
      (interactive "P")
      (if org-agenda-overriding-arguments
          (setq arg org-agenda-overriding-arguments))
      (if (and (stringp arg) (not (string-match "\\S-" arg))) (setq arg nil))
      (let* ((today (org-today))
             (date (calendar-gregorian-from-absolute today))
             (kwds org-todo-keywords-for-agenda)
             (lucky-entry nil)
             (completion-ignore-case t)
             (org-agenda-buffer (when (buffer-live-p org-agenda-buffer)
                                  org-agenda-buffer))
             (org-select-this-todo-keyword
              (if (stringp arg) arg
                (and arg (integerp arg) (> arg 0)
                     (nth (1- arg) kwds))))
             rtn rtnall files file pos marker buffer)
        (when (equal arg '(4))
          (setq org-select-this-todo-keyword
                (org-icompleting-read "Keyword (or KWD1|K2D2|...): "
                                      (mapcar 'list kwds) nil nil)))
        (and (equal 0 arg) (setq org-select-this-todo-keyword nil))
        (catch 'exit
          (org-compile-prefix-format 'todo)
          (org-set-sorting-strategy 'todo)
          (setq files (org-agenda-files nil 'ifmode)
                rtnall nil)
          (while (setq file (pop files))
            (catch 'nextfile
              (org-check-agenda-file file)
              (setq rtn (org-agenda-get-day-entries file date :todo))
              (setq rtnall (append rtnall rtn))))

          (when rtnall
            (setq lucky-entry
                  (nth (random
                        (safe-length
                         (setq entries rtnall)))
                       entries))

            (setq marker (or (get-text-property 0 'org-marker lucky-entry)
                             (org-agenda-error)))
            (setq buffer (marker-buffer marker))
            (setq pos (marker-position marker))
            (org-pop-to-buffer-same-window buffer)
            (widen)
            (goto-char pos)
            (when (derived-mode-p 'org-mode)
              (org-show-context 'agenda)
              (save-excursion
                (and (outline-next-heading)
                     (org-flag-heading nil))) ; show the next heading
              (when (outline-invisible-p)
                (show-entry))           ; display invisible text
              (run-hooks 'org-agenda-after-show-hook))))))

    ;;reset subtask
    (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

    ;; (add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)
    (setq org-return-follows-link t)

    (setq org-plantuml-jar-path
          (expand-file-name "~/.doom.d/plantuml.jar"))
    (setq org-ditaa-jar-path "~/.doom.d/ditaa.jar")

    (require 'org-protocol)
    ;; https://chenzaichun.github.io/post/2021-10-04-org-roam-research-start/


    ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#configuring-doom
    ;; (org-babel-do-load-languages
    ;;  'org-babel-load-languages
    ;;  '((perl . t)
    ;;    (ruby . t)
    ;;    (shell . t)
    ;;    (dot . t)
    ;;    (typescript . t)
    ;;    (js . t)
    ;;    (latex .t)
    ;;    (python . t)
    ;;    (emacs-lisp . t)
    ;;    (plantuml . t)
    ;;    (C . t)
    ;;    (ditaa . t)))

    (progn

      (use-package cal-china-x
        :ensure t
        :demand t)
      (setq mark-holidays-in-calendar t)
      (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
      (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
      (setq calendar-holidays
            (append cal-china-x-important-holidays
                    cal-china-x-general-holidays)))
    (require 'ox-md nil t)

    ;; define the refile targets
    (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
    (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-work (expand-file-name "work.org" org-agenda-dir))
    (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
    (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
    (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
    (setq org-agenda-files (list org-agenda-file-gtd org-agenda-file-journal org-agenda-file-blogposts org-agenda-file-work org-agenda-file-note))

    ;; C-n for the next org agenda item
    (define-key org-agenda-mode-map (kbd "C-p") 'org-agenda-previous-item)


    (with-eval-after-load 'org-agenda
      (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
      ;; 默认显示节假日
      (setq org-agenda-include-diary t)
      )
    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
             "* TODO [#B] %?\n  %i\n %U"
             :empty-lines 1)
            ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
             "* %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
             "* TODO [#B] %?\n  %i\n %U"
             :empty-lines 1)
            ("s" "Slipbox" entry  (file "inbox.org")
             "* %?\n")
            ("S" "Code Snippet" entry
             (file org-agenda-file-code-snippet)
             "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
            ("w" "work" entry (file+headline org-agenda-file-work "Work")
             "* TODO [#A] %?\n  %i\n %U"
             :empty-lines 1)
            ("x" "Web Collections" entry
             (file+headline org-agenda-file-note "Web")
             "* %U %:annotation\n\n%:initial\n\n%?")
            ("p" "Protocol" entry (file+headline org-agenda-file-note "Inbox")
             "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	        ("L" "Protocol Link" entry (file+headline org-agenda-file-note "Inbox")
             "* %? [[%:link][%:description]] \nCaptured On: %U")
            ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
             :empty-lines 1)
            ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n  %i\n %a \n %U"
             :empty-lines 1)
            ("j" "Journal Entry"
             entry (file+datetree org-agenda-file-journal)
             "* %?"
             :empty-lines 1)))

    (with-eval-after-load 'org-capture
      (defun org-hugo-new-subtree-post-capture-template ()
        "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
        (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
               (fname (org-hugo-slug title)))
          (mapconcat #'identity
                     `(
                       ,(concat "* TODO " title)
                       ":PROPERTIES:"
                       ,(concat ":EXPORT_FILE_NAME: " fname)
                       ":END:"
                       "\n\n")          ;Place the cursor here finally
                     "\n")))

      (add-to-list 'org-capture-templates
                   '("h"                ;`org-capture' binding + h
                     "Hugo post"
                     entry
                     ;; It is assumed that below file is present in `org-directory'
                     ;; and that it has a "Blog Ideas" heading. It can even be a
                     ;; symlink pointing to the actual location of all-posts.org!
                     (file+headline org-agenda-file-blogposts "Blog Ideas")
                     (function org-hugo-new-subtree-post-capture-template))))

    ;;An entry without a cookie is treated just like priority ' B '.
    ;;So when create new task, they are default 重要且紧急
    (setq org-agenda-custom-commands
          '(
            ("w" . "任务安排")
            ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
            ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
            ("b" "Blog" tags-todo "BLOG")
            ("p" . "项目安排")
            ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"work\"")
            ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zilongshanren\"")
            ("W" "Weekly Review"
             ((stuck "") ;; review stuck projects as designated by org-stuck-projects
              (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
              ))))

    (add-to-list 'org-agenda-custom-commands
                 '("r" "Daily Agenda Review"
                   ((agenda "" ((org-agenda-overriding-header "今日记录")
                                (org-agenda-span 'day)
                                (org-agenda-show-log 'clockcheck)
                                (org-agenda-start-with-log-mode nil)
                                (org-agenda-log-mode-items '(closed clock state))
                                (org-agenda-clockreport-mode t))))))


    (defvar zilongshanren-website-html-preamble
      "<div class='nav'>
<ul>
<li><a href='http://zilongshanren.com'>博客</a></li>
<li><a href='/index.html'>Wiki目录</a></li>
</ul>
</div>")
    (defvar zilongshanren-website-html-blog-head
      " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
    (setq org-publish-project-alist
          `(
            ("blog-notes"
             :base-directory "~/org-notes"
             :base-extension "org"
             :publishing-directory "~/org-notes/public_html/"

             :recursive t
             :html-head , zilongshanren-website-html-blog-head
             :publishing-function org-html-publish-to-html
             :headline-levels 4         ; Just the default for this project.
             :auto-preamble t
             :exclude "gtd.org"
             :exclude-tags ("ol" "noexport")
             :section-numbers nil
             :html-preamble ,zilongshanren-website-html-preamble
             :author "zilongshanren"
             :email "guanghui8827@gmail.com"
             :auto-sitemap t            ; Generate sitemap.org automagically...
             :sitemap-filename "index.org" ; ... call it sitemap.org (it's the default)...
             :sitemap-title "我的wiki"     ; ... with title 'Sitemap'.
             :sitemap-sort-files anti-chronologically
             :sitemap-file-entry-format "%t" ; %d to output date, we don't need date here
             )
            ("blog-static"
             :base-directory "~/org-notes"
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
             :publishing-directory "~/org-notes/public_html/"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("blog" :components ("blog-notes" "blog-static"))))



    (add-hook 'org-after-todo-statistics-hook 'zilong/org-summary-todo)
    ;; used by zilong/org-clock-sum-today-by-tags

    (define-key org-mode-map (kbd "s-p") 'org-priority)

    (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)

    ;; hack for org headline toc
    (defun org-html-headline (headline contents info)
      "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
      (unless (org-element-property :footnote-section-p headline)
        (let* ((numberedp (org-export-numbered-headline-p headline info))
               (numbers (org-export-get-headline-number headline info))
               (section-number (and numbers
                                    (mapconcat #'number-to-string numbers "-")))
               (level (+ (org-export-get-relative-level headline info)
                         (1- (plist-get info :html-toplevel-hlevel))))
               (todo (and (plist-get info :with-todo-keywords)
                          (let ((todo (org-element-property :todo-keyword headline)))
                            (and todo (org-export-data todo info)))))
               (todo-type (and todo (org-element-property :todo-type headline)))
               (priority (and (plist-get info :with-priority)
                              (org-element-property :priority headline)))
               (text (org-export-data (org-element-property :title headline) info))
               (tags (and (plist-get info :with-tags)
                          (org-export-get-tags headline info)))
               (full-text (funcall (plist-get info :html-format-headline-function)
                                   todo todo-type priority text tags info))
               (contents (or contents ""))
               (ids (delq nil
                          (list (org-element-property :CUSTOM_ID headline)
                                (org-export-get-reference headline info)
                                (org-element-property :ID headline))))
               (preferred-id (car ids))
               (extra-ids
                (mapconcat
                 (lambda (id)
                   (org-html--anchor
                    (if (org-uuidgen-p id) (concat "ID-" id) id)
                    nil nil info))
                 (cdr ids) "")))
          (if (org-export-low-level-p headline info)
              ;; This is a deep sub-tree: export it as a list item.
              (let* ((type (if numberedp 'ordered 'unordered))
                     (itemized-body
                      (org-html-format-list-item
                       contents type nil info nil
                       (concat (org-html--anchor preferred-id nil nil info)
                               extra-ids
                               full-text))))
                (concat (and (org-export-first-sibling-p headline info)
                             (org-html-begin-plain-list type))
                        itemized-body
                        (and (org-export-last-sibling-p headline info)
                             (org-html-end-plain-list type))))
            (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
                  (first-content (car (org-element-contents headline))))
              ;; Standard headline.  Export it as a section.
              (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                      (org-html--container headline info)
                      (org-export-get-reference headline info)
                      (concat (format "outline-%d" level)
                              (and extra-class " ")
                              extra-class)
                      (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                              level
                              preferred-id
                              extra-ids
                              (concat
                               (and numberedp
                                    (format
                                     "<span class=\"section-number-%d\">%s</span> "
                                     level
                                     (mapconcat #'number-to-string numbers ".")))
                               full-text)
                              level)
                      ;; When there is no section, pretend there is an
                      ;; empty one to get the correct <div
                      ;; class="outline-...> which is needed by
                      ;; `org-info.js'.
                      (if (eq (org-element-type first-content) 'section) contents
                        (concat (org-html-section first-content "" info) contents))
                      (org-html--container headline info)))))))))


(use-package org-roam
  :init
  (defun jethro/org-capture-slipbox ()
    (interactive)
    (org-capture nil "s"))
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
    (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
    (setq org-roam-node-display-template (concat "${type:10} ${title:20} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-node-random)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n E" . org-roam-extract-subtree)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

(use-package org-contrib
  :pin nongnu
  :init
  :after org
  (require 'org-checklist))

(use-package org-roam-ui
  :ensure t
  :commands (org-roam-ui-mode)
  :after org
  )

(use-package consult-org-roam
  :ensure nil
  :init
  :commands (consult-org-roam-forward-links)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "s-."))
  :bind
  ("C-c n e" . consult-org-roam-forward-links)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n s" . consult-org-roam-search))

(use-package org-download
  ;; :ensure-system-package (pngpaste . "brew install pngpaste")
  :ensure t
  :demand t
  :after org
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-heading-lvl nil
                org-download-image-dir "./img"
                ;; org-download-screenshot-method "screencapture -i %s"
                org-download-screenshot-method "pngpaste %s"
                org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory)))

(defun dw/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (dw/org-present-prepare-slide)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun dw/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

(defun dw/org-present-prev ()
  (interactive)
  (org-present-prev)
  (dw/org-present-prepare-slide))

(defun dw/org-present-next ()
  (interactive)
  (org-present-next)
  (dw/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-j" . dw/org-present-next)
         ("C-c C-k" . dw/org-present-prev))
  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode-quit . dw/org-present-quit-hook)))

;; https://github.com/turbana/emacs-config/blob/2003077c60ffabd3b48835727af8953fc37067f7/README.org#zetian
(provide 'init-org)
