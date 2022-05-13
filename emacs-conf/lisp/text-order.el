;;; text-order.el --- Selects menu meals for each day of month.

;; Copyright (C) 2018, Programes per a Casa.

;; Author: Gustavo Puche <gustavo.puche@gmail.com>
;; Created: 23 Jul 2018
;; Version: 0.1
;; Keywords: list order
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Programming aling tools.

;; (number-sequence ini n) creates a list of n numbers begining with ini
;; (nth (random 7) text-order-dies)
;; (setq new-list (append list nil)) copy of list in new-list. NOT REFERENCE of list.

;;; Code:
(defun align-repeat (start end regexp)
  "Repeat alignment with respect to 
     the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end 
				(concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun align-repeat-0 (start end regexp)
  "Repeat alignment with respect to 
     the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end 
				(concat "\\(\\s-*\\)" regexp) 1 0 t))

;; SQL varibles
(defun align-var-sql (start end)
  "Align SQL varibles."
  (interactive "r") ;; Pass region as arguments
  (let ((case-fold-search nil)) ;; Disable no case search locally.
  (align-regexp start end
				"\\(\\s-+\\)\\(:=\\|[[:upper:]][[:upper:]]\\)" 1 1 t)))

;; SQL varibles
(defun align-var-cpp (start end)
  "Align C++ varibles."
  (interactive "r") ;; Pass region as arguments
  (let ((case-fold-search nil)) ;; Disable no case search locally.
  (align-regexp start end
				"\\(\\s-+\\)\\(=\\|[[:lower:]][[:upper:]]\\|*[[:lower:]]\\)" 1 1 t)))

(provide 'text-order)

;;; text-order.el ends here
