;;; corfu-english-helper.el --- English helper with corfu interface

;;; Commentary:
;;
;; English helper with corfu interface.
;;

;;; Require
(require 'cl-lib)
(require 'corfu-english-helper-data)

;;; Code:

(defvar corfu-english-helper-active-p nil
  "The status of corfu-english-helper plugins.
Default is disable.")

(defvar corfu-english-helper-last-showing-candidate-max-index nil)
(defvar corfu-english-helper-current-input nil)
(defvar corfu-english-helper-translation-max-width nil)

(defun corfu-english-helper-translation-max-width ()
  (let ((showing-candidate-max-index (min (+ corfu-count corfu--scroll) corfu--total))
        (showing-candidate-min-index corfu--scroll))
    (if (and (eq corfu-english-helper-current-input corfu--input)
             (eql showing-candidate-max-index corfu-english-helper-last-showing-candidate-max-index))
        corfu-english-helper-translation-max-width
      (progn
        (setq corfu-english-helper-last-showing-candidate-max-index showing-candidate-max-index)
        (setq corfu-english-helper-current-input corfu--input)
        (setq showing-candidates (butlast (nthcdr showing-candidate-min-index corfu--candidates) (- corfu--total showing-candidate-max-index)))
        (setq corfu-english-helper-translation-max-width
              (apply 'max (mapcar (lambda (w)
                                    (string-width (get-text-property
                                                   0
                                                   :initials (assoc-string (downcase w) corfu-english-helper-completions))))
                                  showing-candidates)))))))

(defun corfu-english-helper-annotation (candidate)
  (let* ((translation (get-text-property 0 :initials candidate))
         (translation-width (string-width translation))
         (corfu--candidates-length (length corfu--candidates))
         (max-translation-width (corfu-english-helper-translation-max-width))
         (blank-width (- max-translation-width translation-width)))
    (format "    %s" (concat translation (make-string (max 0  (- max-translation-width translation-width)) ?\s)))))

(defun corfu-english-helper-search (&optional interactive)
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list 'corfu-english-helper-search)))
        (completion-at-point))
    (let* ((bds (bounds-of-thing-at-point 'symbol))
           (start (car bds))
           (end (cdr bds))
           (prefix (if (and start end)
                       (buffer-substring-no-properties start end)
                     "")))
      (list start end (corfu-english-helper-get-items prefix)
            :annotation-function 'corfu-english-helper-annotation))))

(defun corfu-english-helper-get-items (prefix)
  (let* ((prefix-match-candidates
          (cl-remove-if-not
           (lambda (c)  (string-prefix-p (downcase prefix) c))
           corfu-english-helper-completions)))
    (corfu-english-helper-convert-candidates prefix prefix-match-candidates)))

(defun corfu-english-helper-convert-candidates (input candidates)
  (cond ((corfu-english-helper-upcase-string-p input)
         (mapcar 'upcase candidates))
        ((corfu-english-helper-capitalize-string-p input)
         (mapcar 'capitalize candidates))
        (t candidates)))

(defun corfu-english-helper-upcase-string-p (str)
  (let ((case-fold-search nil))
    (and (> (length str) 1)
         (string-match-p "\\`[A-Z]*\\'" str))))

(defun corfu-english-helper-capitalize-string-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z][a-z]*\\'" str)))

(defun toggle-corfu-english-helper ()
  "Toggle corfu english helper."
  (interactive)
  (if corfu-english-helper-active-p
      (progn
        ;; Restore options.
        (setq-local corfu-auto corfu-english-helper--corfu-auto)
        (setq-local corfu-auto-prefix corfu-english-helper--corfu-auto-prefix)

        (setq completion-at-point-functions (remove 'corfu-english-helper-search completion-at-point-functions))
        (setq corfu-english-helper-active-p nil)
        (message "Corfu english helper has disable."))

    ;; Save options.
    (setq-local corfu-english-helper--corfu-auto corfu-auto)
    (setq-local corfu-english-helper--corfu-auto-prefix corfu-auto-prefix)

    ;; Turn on `corfu-auto' and adjust `corfu-auto-prefix' to 0.
    (setq-local corfu-auto t)
    (setq-local corfu-auto-prefix 0)

    ;; We need call `(setq-local corfu-auto t)' before corfu-mode turn on.
    (corfu-mode 1)

    (add-to-list 'completion-at-point-functions #'corfu-english-helper-search)
    (setq corfu-english-helper-active-p t)
    (message "Corfu english helper has enable.")))

(provide 'corfu-english-helper)

;;; corfu-english-helper.el ends here
