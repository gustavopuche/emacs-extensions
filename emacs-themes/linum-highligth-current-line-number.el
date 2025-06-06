;; Copyright (C) 2014 Emanuele Tomasi <targzeta@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;; Author: Emanuele Tomasi <targzeta@gmail.com>
;; Version: 0.1
;; Maintainer: Emanuele Tomasi <targzeta@gmail.com>
;; Keywords: convenience

;;; Commentary:
;; A function for the `linum-format' variable to highlight the current line
;; number with a custom face.

;;; Changelog:
;; 2015/08/25
;; bugfix: too slow. linum-highlight-current-line is called for every line to
;;         display, linum-update on the other way is called only on update. So
;;         we moved all the expansive operations to the latter function.
(require 'linum)

(defvar linum-current-line                        1         "Current line number.")
(defvar linum-digits                              3         "Digits count for linum.")
(defvar linum-right-border-width-before-delimiter 0         "Right border width before delimiter for linum.")
(defvar linum-right-border-width-after-delimiter  0         "Right border width after delimiter for linum.")
(defvar linum-left-border-width                   0         "Left border width for linum.")
(defvar linum-number-and-code-delimiter           "\u2502"  "Delimiter between line numbers and code.")

(defface linum-current-line
  `((t :inherit linum
       :foreground "goldenrod"
       :weight bold
       ))
  "Face for displaying the current line number."
  :group 'linum)

(defadvice linum-update (before advice-linum-update activate)
  "Set the current line."
  (setq linum-current-line (line-number-at-pos)
        ;; It's the same algorithm that linum dynamic. I only had added one
        ;; space in front of the first digit.
        linum-digits (number-to-string
                      (+ linum-left-border-width (length
                       (number-to-string
                        (count-lines (point-min) (point-max))))))))

(defun linum-highlight-current-line (line-number)
  "Highlight the current line number using `linum-current-line' face."
  (let ((face (if (= line-number linum-current-line)
                  'linum-current-line
                'linum)))
    (propertize (format (concat "%" linum-digits "d" (make-string linum-right-border-width-before-delimiter ? ) linum-number-and-code-delimiter (make-string linum-right-border-width-after-delimiter ? ) ) line-number)
                'face face)))

(setq linum-format 'linum-highlight-current-line)
