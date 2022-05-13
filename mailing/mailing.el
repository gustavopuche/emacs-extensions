;;; mailing.el --- List decision with constraints defined by user.

;; Copyright (C) 2019, Inference motor.

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
;; Lists with constraints defined by user.
;;
;; MAILING
;;
;; Execution
;;
;; (load-list-files)

;;; Code

;; Global variables
(defvar address-list nil
  "List of addresses")

(defvar addresses-file "/home/gustavo/org/brain/CGC/PRD/cartes/addresses.org"
  "Addresses org file")

(defvar header-file "/home/gustavo/org/brain/CGC/PRD/cartes/header.tex"
  "Latex header file")

(defvar begin-letter-file "/home/gustavo/org/brain/CGC/PRD/cartes/begin.tex"
  "Latex begin letter file")

(defvar body-file "/home/gustavo/org/brain/CGC/PRD/cartes/body.tex"
  "Latex body end letter file")

(defvar ending-file "/home/gustavo/org/brain/CGC/PRD/cartes/ending.tex"
  "Latex ending letter file")

(defvar pdf-filename ""
  "PDF filename processed by mailing.")

;;###############################################################################
;; READ ADDRESSES
;;###############################################################################
(defun read-addresses (filePath)
  "Read addresses separated by +"
  (progn
    (setq address-list "")
    (with-temp-buffer
      (insert-file-contents filePath)
      (let ((fileList (split-string (buffer-string) "\n" t))
	    (address))
	(dolist (elt fileList)
	  (if (equal elt "+")
	      (progn
		(print "Fi")
		(setq address-list (append address-list (list address)))
		(setq address nil))
	    (progn
	      (print elt)
	      (setq address (append address (list elt))))))))))
  

(defun string-address (list)
  "Split list address into string address"
  (if (not (null list))
	   (concat (car list) "\\\\\n" (string-address (cdr list)))))

(defun insert-addresses (list)
  ""
  (if (not (null list))
      (progn
	(insert "\\begin{letter}{%\n")
	(insert (string-address (car list)))
	(insert-file-contents body-file)
	(end-of-buffer)
	(insert-addresses (cdr list)))))


(defun compose-letters (filePath outputFile)
  (progn
    (read-addresses filePath)
    (with-temp-buffer
      (insert-file-contents header-file)
      (end-of-buffer)
      (insert-addresses address-list)
      (insert "\\end{document}\n")
      (write-file outputFile))
    (compile (concat "pdflatex -shell-escape -interaction nonstopmode " outputFile))
    (setq pdf-filename (concat (file-name-sans-extension outputFile) ".pdf"))))

(defun my-compilation-finish-function (buffer desc)
  (message "Opening file %s" pdf-filename)
  (find-file pdf-filename))

;; (add-hook 'compilation-finish-functions 'my-compilation-finish-function)

(provide 'mailing)

;;; mailing.el ends here
