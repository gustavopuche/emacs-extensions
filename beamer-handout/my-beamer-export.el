;;; compile-tools.el --- Beamer hanouts settings

;; Copiright (C) 2020 Gustavo Puche

;; Author: Gustavo Puche <gustavo.puche@gmail.com>
;; Created: 30 July 2020
;; Version: 0.1
;; Keywords: languages latex
;; Package-Requires:


;;; Commentary:
;; Settup beamer handouts and slides

;;; Code:
(defun export-slides-or-handouts (tagname handouts)
  "Export org subtree with tag tagname as beamer slides or handouts. If handout
is non-nil, exports to a file named tagname-handouts.tex and uses \"[handout]\"
as the class option. Otherwise, export to a file named tagname-slides.tex with
empty class option. Any underscore in tagname is replaced by a dash in the file
name."
  (save-excursion
    (beginning-of-buffer)
    ; Search for tagname
    (search-forward (concat ":" tagname ":"))
    (let ((org-export-select-tags (list tagname))
          (async nil)
          (subtreep t)
          (visible-only nil)
          (body-only nil)
          ; Use [handout] option if exporting handouts, otherwise empty.
          (ext-plist (if handouts '(:latex-class-options "[handout]")
                       '()))
          ; Export name is tagname (with underscores replaced by dashes)
          ; followed
          (export-name (concat (subst-char-in-string ?_ ?- tagname)
                               (if handouts "-handouts" "-slides")
                               ".tex")))
      (org-export-to-file 'beamer export-name async subtreep
          visible-only body-only ext-plist
          (lambda (file) (org-latex-compile file))))
    )
  )

(defun get-current-lecture-tag()
  "Return the first tag that appears together with the first :cur: tag in the file."
  (save-excursion
    (beginning-of-buffer)
    ; Search for first :cur: tag.
    (if (search-forward ":cur:" nil t)
        ; Return first tag in list, excluding :cur:
        (nth 0 (remove "cur" (org-get-tags)))
      (error (message "No :cur: tag found.")))))

(defun export-current-slides()
  "Export the subtree with the :cur: tag as beamer slides."
  (interactive)
  (let ((tag (get-current-lecture-tag)))
    (if tag
        (progn (message "Exporting %s slides" tag)
               (export-slides-or-handouts tag nil))
      (error (message "No tag on :cur: lecture")))))

(defun export-current-handouts()
  "Export the subtree with the :cur: tag as beamer handouts."
  (interactive)
  (let ((tag (get-current-lecture-tag)))
    (if tag
        (progn (message "Exporting %s handouts" tag)
               (export-slides-or-handouts tag t))
      (error (message "No tag on :cur: lecture")))))

(defun set-my-beamer-export-keys()
  (interactive)
  (local-set-key (kbd "<f12>") 'export-current-slides)
  (local-set-key (kbd "<S-f12>") 'export-current-handouts))

(provide 'my-beamer-export)

;;; my-beamer-export.el ends here
