;;; menjars-mes.el --- Selects menu meals for each day of month.

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
;; Read from file menus and do a list of meals for each week day.

;; (number-sequence ini n) creates a list of n numbers begining with ini
;; (nth (random 7) menjars-mes-dies)
;; (setq new-list (append list nil)) copy of list in new-list. NOT REFERENCE of list.

;; Get a list from a string:
;;
;;     (eval (intern "dies"))

;;; Code:

(defconst menjars-mes-dies '(Dilluns Dimaig Dimecres Dijous Divendres Disabte Dumenge) "Day names of the week.")

(defconst menjars-mes-plats '("Paella" "Arros en abaejo" "Arrós blanc" "Tortilla de quereguilles"
							  "Pollastre al forn" "Quereguilles al forn" "Quiche" "Espaguetis"
							  "Macarrons") "List of eating.")

(defconst menjars-mes-aliment '( (Peix 2) (Carn 1) (Llegums 2) (Cereals 2) ))

(defun menjars-mes-listing (list)
  (dolist (elt list)
	(print elt)))

(defun menjars-mes-listing2 (list)
  (let (value)
  (dolist (element list value)
	(setq value (append element value)))))

(defun menjars-mes-reverse (list)
  (let (value)
	(dolist (element list value)
	  (setq value (cons element value))
	  (message "Element= %s Valor= %s." element value))))

(defun menjars-mes-read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun make-data-from-file (filePath)
  (let (lines (list-read-lines filePath))
	(dolist (element lines value)
	  )))

(defun menjars-read-list (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
	(let ((fileList (split-string (buffer-string) "\n" t)))
	  (cond ((equal (car fileList) "Peix") (progn (print "Es una lista de Peix")
												  (setq Peix (cdr fileList))))
			((equal (car fileList) "Carn") (progn (print "Es una lista de Carn")
												  (setq Carn (cdr fileList))))
			((equal (car fileList) "Llegums") (progn (print "Es una lista de Llegums")
												  (setq Llegums (cdr fileList))))
			((equal (car fileList) "Cereals") (progn (print "Es una lista de Cereals")
												  (setq Cereals (cdr fileList))))
			(t (progn (print "No es una lista Coneguda")
			   fileList))
		)
	  )
	))

;; Inits data
(defun init-lists ()
	(progn	(setq dies (append menjars-mes-dies nil))
			(setq plats (append menjars-mes-plats nil))
			(setq plats-escollits nil)))

;; Gets random elment from list.
(defun random-list (list)
  (nth (random (length list)) list))

(defun random-plats ()
  (nth (random (length plats)) plats))

(defun agafa-plat ()
  (let ((plat (random-plats)))
	(delete plat plats)
	(setq plats-escollits (append (append (list plat) plats-escollits) nil))
	(message "El plat seleccionat es %s" plat)
	))

;; Returns true if element is in list false otherwise.
(defun element-in-list (element list)
  (if (member element list)
	  element
	nil))

;; Returns list with the elements presents in the 2 lists given.
(defun elements-in-other-list (list1 list2)
  (if (not (null list1))
	  (if (member (car list1) list2)
		  (append (list (car list1)) (elements-in-other-list (cdr list1) list2))
		(append nil (elements-in-other-list (cdr list1) list2)))
	nil))

;; Visits list of tow elements (elemnet number) and display result.
(defun two-element-list-random (list)
  (if (not (null list))
	  (let ((idx (random (length list)))
			(value)
			(res nil))
		(setq value (nth idx list))
		(if (= (car (cdr value)) 1)
			(progn
			  (print value)
			  (two-element-list-random (delete value list)))
		  (progn
			(print value)
			(setcar (nthcdr idx list) (list (car value) (- (car (cdr value)) 1)))
			(two-element-list-random list))
		  )
		)
	nil))

;; Returns greather list.
(defun longest-list (list1 list2)
  "Returns greather list."
  (> (length list1) (length list2)))

;; Returns list of list ordered by len
(defun sort-by-len (list)
  "Returns list of list ordered by len"
  (sort list 'longest-list))

;;****************************************************************************;;
;;                                                                            ;;
;;                       INITIALIZE VARIABLES                                 ;;
;;                                                                            ;;
;;****************************************************************************;;

;;----------------------------------------------------------------------------;;
;; Parse text files into list of lists.
;;
;; TAB is first list separator.
;; ,   is second list separator.
;;
;; Arguments.
;;  filePath   - String file path/file name.
;;
;; Returns.
;;  res        - List of file data.
;;----------------------------------------------------------------------------;;
(defun list-read-lines (filePath)
  "Parse text files into list of lists.

   TAB is first list separator.
   ,   is second list separator.

   Arguments.
    filePath   - String file path/file name.

   Returns.
    res        - List of file data."
  (with-temp-buffer
    (insert-file-contents filePath)
    (let ((fileList (split-string (buffer-string) "\n" t))
		  (res)
		  (res1)
		  (res2))
	  (dolist (elt fileList)
		(setq res1 (split-string elt "\t" t))
		;;(setq res (append res (list (split-string elt "\t" t))))
		(dolist (elt2 res1)
		  (setq res2 (append res2 (list (split-string elt2 "," t))))
		  (print res2)
		  )
		(setq res (append res (list res2)))
		(setq res2 nil)
		(print res)
		)
	  ;; Return value
	  res
	  )
	)
  )

;;----------------------------------------------------------------------------;;
;; Loads lists from files
;;
;; 'Programacio
;; 'Germans
;; 'Restriccions
;;----------------------------------------------------------------------------;;
(defun load-lists-files ()
  "Loads lists from files
   'Programacio
   'Germans
   'Restriccions"
  (setq Programacio (list-read-lines "../../assignacions/programacio.txt"))
  (setq Germans (list-read-lines "../../assignacions/germans.txt"))
  (setq Restriccions (list-read-lines "../../assignacions/restriccions.txt"))
  (setq Assignar Germans)
  (setq Acompanyar Germans)
  (setq Seleccio nil) ;; List of constraint that matches current person assigned.
  (setq Resultat nil)
  (restric-list Restriccions)
  )

;;****************************************************************************;;
;;                                                                            ;;
;;                          AUX FUNCTIONS                                     ;;
;;                                                                            ;;
;;****************************************************************************;;

;;----------------------------------------------------------------------------;;
;; Gets name of list
;;
;; The name is the 1st element.
;;----------------------------------------------------------------------------;;
(defun get-list-name (list)
  "Gets name of list

   The name is the 1st element."
  (car (car list))
  )

;;----------------------------------------------------------------------------;;
;; Gets value of first element in list
;;
;; The value is the 1st element.
;;----------------------------------------------------------------------------;;
(defun get-list-value (list)
  "Gets value of first element in list

   The value is the 1st element."
  (car (cdr (car list)))
  )

;;----------------------------------------------------------------------------;;
;; Gets qualities of person.
;;
;; The qualities are list from 2nd element until end.
;;----------------------------------------------------------------------------;;
(defun get-list-qualities (list)
  "Gets qualities of person.

   The qualities are list from 2nd element until end."
  (cdr list)
  )

;;----------------------------------------------------------------------------;;
;; Gets constraint list from quality-list-name
;;
;; Returns constraint list.
;;----------------------------------------------------------------------------;;
(defun get-list-constraints (name)
  "Gets constraint list from quality-list-name

   Returns constraint list."
  (eval (intern name))
  )

;;----------------------------------------------------------------------------;;
;; Gets List element
;;
;; Arguments.
;;  constraints.
;,
;; Returns.
;;  Element in list.
;;----------------------------------------------------------------------------;;
(defun get-first-cons-element (constraints)
  "Gets List element

   Arguments.
    List
    String element - 1

   Returns.
    Element in list."
  (nth (- (string-to-number (get-list-value constraints)) 1) (get-list-constraints (get-list-name constraints)))
  )

;;----------------------------------------------------------------------------;;
;; Looks for quality name.
;;
;; Arguments.
;;  list    - Quality list.
;;  quality - Quality string name.
;;
;; Returns.
;;  Quality list.
;;----------------------------------------------------------------------------;;
(defun get-quality (list quality)
  "Looks for quality name.

   Arguments.
    list    - Quality list.
    quality - Quality string name.

   Returns.
    Quality list."
  (if (not (null list))
	  (if (equal quality (get-list-name list))
		  (car list)
		(get-quality (cdr list) quality)
		  )
	nil
	)
  )

;;----------------------------------------------------------------------------;;
;; Gets Value of selected quality.
;;
;; Arguments.
;;  quality       - (quality-name quality-value).
;; Returns.
;;  quality-value.
;;----------------------------------------------------------------------------;;
(defun get-quality-value (quality)
  "Gets Value of selected quality.

   Arguments.
    quality       - (quality-name quality-value).
   Returns.
    quality-value."
  (car (cdr quality))
  )

;;----------------------------------------------------------------------------;;
;; Adds list1 element to list2.
;;----------------------------------------------------------------------------;;
(defun add-doubled-list (list1 list2)
  "Adds list1 element to list2."
  (append (list list1) list2)
  )


;;----------------------------------------------------------------------------;;
;; Get first occurrence of 
;;----------------------------------------------------------------------------;;
(defun get-list-index (list quality index)
  (if (equal quality (get-list-name list))
	  (+ index 1)
	(get-list-index((cdr list) quality (+ index 1)))
	)
  )

;; Adds to Seleccio person qualities
(defun store-remaining-qualities (list)
  ""
  (if (not (null list)) ;; Stop condition.
	  (let ((cons-list (get-list-constraints(get-list-name(list))))
			(quality-value (get-list-value list)))
		(setq Seleccio (add-doubled-list (list cons-name (number-to-string (get-list-index cons-list quality-name 0))) Seleccio))
		(store-remaining-qualities (cdr list))
		)
	)
  )

;;----------------------------------------------------------------------------;;
;; Chechs if all constraints apply to person qualities.
;;
;; Returns t or nil
;;----------------------------------------------------------------------------;;
(defun suitable-person (qualities constraints)
  "Chechs if all constraints apply to person qualities.

   Returns t or nil"
  (let ((cons-name (get-list-name constraints))
		(cons-elem (get-first-cons-element constraints))
		(person-quality)
		(netx-constraints)
		(next-qualities))
	(setq person-quality (get-quality qualities cons-name))
	;; If person fills current constraint.
	(if (equal (get-quality-value person-quality) (car cons-elem))
		(progn
		  (setq next-constaints (cdr constraints))
		  (setq next-qualities (remove person-quality qualities))
		  (setq Seleccio (add-doubled-list (car constraints) Seleccio)) ;; Adds selectioned constraint.
		  (if (null next-constaints)
			  (progn
				(store-remaining-qualities next-qualities) ;; Adds to Seleccio person remaining qualities.
				t)
			suitable-person (next-qualities next-constraints)
			)
		  )
	  nil
	  )
	)
  )

;;----------------------------------------------------------------------------;;
;; Checks if person fills all contraints.
;;
;; Returns t or nil.
;;----------------------------------------------------------------------------;;
(defun if-fill-constraints (person constraints)
  "Checks if person fills all contraints.

   Returns t or nil."
  ;; TODO: Fix Constraints list Idioma.
  (let ((qualities (get-list-qualities person)))
	(if (suitable-person qualities constraints)
		t
	  nil)
	)
  )

;;----------------------------------------------------------------------------;;
;; Selects person that fills all contraints.
;;
;; Arguments.
;;  list        - List of all remaining persons to assign.
;;  constraints - List of constraint for assignement.
;;
;; Returns
;;  list        - Person selected and all his qualities.
;;----------------------------------------------------------------------------;;
(defun select-assigned (list constraints)
  "Selects person that fills all contraints.

   Arguments.
    list        - List of all remaining persons to assign.
    constraints - List of constraint for assignement.

   Returns
    list        - Person selected and all his qualities."
  (if (not (null list))
	  (if (if-fill-constraints (car list) constraints)
		  (car list) ;; Returns  selected person.
		(select-assigned (car list) constraints)
		)
	nil) ;; Returns nil if nobody fills constraints.
  )

;;----------------------------------------------------------------------------;;
;; Decompose list
;;----------------------------------------------------------------------------;;
;;
;; Extract lists from list of lists.
;;
;; First Element is name.
;; Other elements are list elements.
;;----------------------------------------------------------------------------;;
(defun decompose-list (list)
  (set (intern (car (car list))) (cdr list))
  )

;;----------------------------------------------------------------------------;;
;; Creates list variables for all element in list.
;;
;; Calls decompose-list for all element in list.
;;----------------------------------------------------------------------------;;
(defun restric-list (list)
  "Creates list variables for all element in list.

   Calls decompose-list for all element in list."
  (if (not (null list))
	  (progn
		(decompose-list (car list))
		(restric-list (cdr list))
		)
	)
  )

;;----------------------------------------------------------------------------;;
;; Assign the given assignament to brother or sister.
;;
;; Called from assign-week (list).
;;----------------------------------------------------------------------------;;
(defun assign-assignement (list)
  "Assign the given assignament to brother or sister.

   Called from assign-week (list)."
  (if (boundp (intern (car list)))
	  (print (car list))
	  )
  )

;;----------------------------------------------------------------------------;;
;; Assign week in function of Restrictions.
;;
;; Called from main-program-assignements.
;;----------------------------------------------------------------------------;;
(defun assign-week (list)
  "Assign week in function of Restrictions.

   Called from main-program-assignements."
  (if (not (null list))
	  (progn
		(assign-assignement (car list))
		(assign-week (cdr list))
		)
	)
  )

;;****************************************************************************;;
;;                                                                            ;;
;;                              MAIN                                          ;;
;;                                                                            ;;
;;****************************************************************************;;

;;----------------------------------------------------------------------------;;
;; Main program of Assignements
;;
;; Walks into Programacio and set Assignements.
;;----------------------------------------------------------------------------;;
(defun main-program-assignements (list)
  "Walks into Programacio and set Assignements."
  (if (not (null list))
	  (progn
		(assign-week (car list))
		(main-program-assignements (cdr list))
		)
	)
  )

;;****************************************************************************;;
;;                                                                            ;;
;;                             OUTPUT                                         ;;
;;                                                                            ;;
;;****************************************************************************;;
(defun x-make-word-red (begin end)
  "make current region colored red, using text properties"
  (interactive "r")
  (put-text-property begin end 'font-lock-face '(:foreground "red")))

(defun color-text-insert (text)
  "Inserts text with color at current buffer."
  (insert (propertize text 'font-lock-face '(:foreground "orange"))))

(defun color-text (color text)
  "Inserts text with color at current buffer."
  (insert (propertize text 'font-lock-face '(:foreground ,color)))
  (message color))


;;****************************************************************************;;
;;                                                                            ;;
;;                              DEBUG                                         ;;
;;                                                                            ;;
;;****************************************************************************;;

;;----------------------------------------------------------------------------;;
;; See directory from package.
;;----------------------------------------------------------------------------;;
(defun lists-dir ()
  "See directory from package."
  (pwd)
  )

;;----------------------------------------------------------------------------;;
;; Debug function.
;; Print list and list of lists.
;;----------------------------------------------------------------------------;;
(defun print-list (list)
  "Print list and list of lists."
  (if (not (null list))
	  (if (listp (car list))
		  (progn
			(print-list (car list))
			(print-list (cdr list))
			)
		(progn
		  (print (car list))
		  (print-list (cdr list))
		  )
		)
	)
  )		

(provide 'menjars-mes)

;;; menjars-mes.el ends here
