;;; licondefusr.el --- List decision with constraints defined by user.

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
;; LICONDEFUSR
;;
;; Execution
;;
;; (load-list-files)

;;; Code

;;****************************************************************************;;
;;                                                                            ;;
;;                       INITIALIZE VARIABLES                                 ;;
;;                                                                            ;;
;;****************************************************************************;;

;;----------------------------------------------------------------------------;;
;; Load list from file
;;----------------------------------------------------------------------------;;
(defun load-result (filePath)
    (with-temp-buffer
      (insert "(setq Resultat '")
      (insert-file-contents filePath)
      (goto-char (point-max))
      (insert ")")
	  (eval-buffer)
	  )
    )

(defun load-list (list-name filePath)
    (with-temp-buffer
      (insert "(setq ")
	  (insert list-name)
	  (insert " '")
      (insert-file-contents filePath)
      (goto-char (point-max))
      (insert ")")
	  (eval-buffer)
	  )
    )

;;----------------------------------------------------------------------------;;
;; Saves list into file.
;;----------------------------------------------------------------------------;;
(defun save-list (list filePath)
  (with-output-to-temp-buffer "*new*"
	(print list)
	(switch-to-buffer "*new*")
	(write-file filePath)
	(previous-buffer))
  )

;;----------------------------------------------------------------------------;;
;; Saves results for month
;;----------------------------------------------------------------------------;;
(defun save-execution-results (month)
  (save-list Resultat (concat "/home/gustavo/assignacions/resultat-" month ".txt"))
  (save-list Assignar (concat "/home/gustavo/assignacions/assignar-" month ".txt"))
  (save-list Acompanyar (concat "/home/gustavo/assignacions/acompanyar-" month ".txt"))
  )

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
(defun load-lists-files (month)
  "Loads lists from files
   'Programacio
   'Germans
   'Restriccions"
  (setq Programacio (list-read-lines (concat "/home/gustavo/assignacions/programacio-" month ".txt")))
  (setq Germans (list-read-lines "/home/gustavo/assignacions/germans.txt"))
  (setq Restriccions (list-read-lines (concat "/home/gustavo/assignacions/restriccions-" month ".txt")))
  ;;(setq Assignar (append Germans nil))   ;; Copy of brothers list.
  (load-list "Assignar" (concat "/home/gustavo/assignacions/assignar-" month ".txt"))
  ;;(setq Acompanyar (append Germans nil)) ;; Copy of brothers list.
  (load-list "Acompanyar" (concat "/home/gustavo/assignacions/acompanyar-" month ".txt"))
  (setq Seleccio nil) ;; Constraints current person assigned. Need for partner.
  ;;  (setq Resultat nil)
  (load-result (concat "/home/gustavo/assignacions/resultat-" month ".txt"))
  (setq Singles nil)
  (setq assigned-this-week nil) ;; List of assigned people to no repeat same week.
  (restric-list Restriccions) ;; Setq list of constraints by name.
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
	  index
	(get-list-index (cdr list) quality (+ index 1))
	)
  )

;;----------------------------------------------------------------------------;;
;; Adds to Seleccio person qualities
;;----------------------------------------------------------------------------;;
(defun store-remaining-qualities (list)
  "Returns doubled list with remaining qualities of selected person.

   Example ((Idioma 1))"
  (if (not (null list)) ;; Stop condition.
	  (let ((quality-name (get-list-name list))
			(cons-list (get-list-constraints(get-list-name list)))
			(quality-value (get-list-value list)))
		(setq Seleccio (add-doubled-list (list quality-name (number-to-string (get-list-index cons-list quality-value 1))) Seleccio))
		(store-remaining-qualities (cdr list))
		)
	)
  )


;;----------------------------------------------------------------------------;;
;; Looks for couple of assigned and partner in Result.
;;----------------------------------------------------------------------------;;
(defun are-in-result (list person1 person2)
  (if (not (null list))
	  (if (equal (car (cdr (car list))) (list person1 person2))
		  t
		(are-in-result (cdr list) person1 person2)
		)
	nil)
  )

;;----------------------------------------------------------------------------;;
;; Looks for single assigned in Result.
;;----------------------------------------------------------------------------;;
(defun is-in-result (list person)
  (if (not (null list))
	  (if (equal (car (cdr (car list))) (list person))
		  t
		(is-in-result (cdr list) person)
		)
	nil)
  )

;;----------------------------------------------------------------------------;;
;; Gets the number of partners of person.
;;----------------------------------------------------------------------------;;
(defun number-of-partners (list person number)
  (if (not (null list))
	  (if (equal (car (car (cdr (car list)))) person)
		  (number-of-partners (cdr list) person (+ number 1))
		(number-of-partners (cdr list) person number))
	number)
  )


;;----------------------------------------------------------------------------;;
;; Returns constraint value 1 or 2
;;
;; Example:
;;   (get-constraint-value '("Basic" "NIL") t) --> "Basic"
;;
;;   (get-constraint-value '("Basic" "NIL") nil) --> nil
;;----------------------------------------------------------------------------;;
(defun get-constraint-value (constraint is-assigned)
  (if is-assigned
	  (if (equal (car constraint) "NIL")
		  nil
		(car constraint)
		)
	(if (equal (car (cdr constraint)) "NIL")
		nil
	  (car (cdr constraint))
	  )
	)
  )

;;----------------------------------------------------------------------------;;
;;----------------------------------------------------------------------------;;
(defun is-person-number-before (list person number)
  (if (equal number 0)
	  nil
	(if (equal person (car (car (cdr (nth (- (length list) number) list)))))
		t
	  (if (equal person (car (cdr (car (cdr (nth (- (length list) number) list))))))
		  t
		(is-person-number-before list person (- number 1))
		)
	  )
	)
  )

;;----------------------------------------------------------------------------;;
;; Checks if all constraints apply to person qualities.
;;
;; is-assigned:
;;   t   if is person to assign.
;;   nil if is partner.
;;
;; Returns t or nil
;;----------------------------------------------------------------------------;;
(defun suitable-person (qualities constraints is-assigned)
  "Chechs if all constraints apply to person qualities.

   Returns t or nil"
  (let ((cons-name (get-list-name constraints))
		(cons-elem (get-first-cons-element constraints))
		(person-quality)
		(next-constraints)
		(next-qualities))
	(setq person-quality (get-quality qualities cons-name))
	;; If person fills current constraint.        This gets firt element of constraint.
	(if (equal (get-quality-value person-quality) (get-constraint-value cons-elem is-assigned)) ;; Ex: "Bàsic" from ("Bàsic" "NIL").
		(progn
		  (setq next-constraints (cdr constraints))
		  (setq next-qualities (remove person-quality qualities))
		  (if is-assigned
			  (setq Seleccio (add-doubled-list (car constraints) Seleccio)) ;; Adds selectioned constraint.
			)
		  (if (null next-constraints)
			  (progn
				(if is-assigned
					(store-remaining-qualities next-qualities) ;; Adds to Seleccio person remaining qualities.
				  )
				t)
			(suitable-person next-qualities next-constraints is-assigned)
			)
		  )
	  nil
	  )
	)
  )

;;----------------------------------------------------------------------------;;
;; Checks if person fills all contraints.
;;
;; Arguments.
;;  person      - Person to check all constraints.
;;  constraints - Assignment constraints.
;;  is-assigned - If t looks for assigned person if nil looks for partner.
;;
;; Returns t or nil.
;;----------------------------------------------------------------------------;;
(defun if-fill-constraints (person constraints is-assigned)
  "Checks if person fills all contraints.

   Arguments.
    person      - Person to check all constraints.
    constraints - Assignment constraints.
    is-assigned - If t looks for assigned person if nil looks for partner.

   Returns t or nil."
  ;; (if (member person assigned-this-week)
  (if (is-person-number-before Resultat person 8)
	  (progn
		(princ "--- Person is befor in Result table: ")
		(princ person)
		(princ "\n")
		nil ;; Fails if he/she is allready assigned.
		)
	(let ((qualities (get-list-qualities person)))
	  (if (suitable-person qualities constraints is-assigned)
		  t
		(progn
		  (princ person)
		  (princ " does NOT fills ")
		  (if (not is-assigned)
			  (princ "partner "))
		  (princ "constraints: ")
		  (princ constraints)
		  (princ "\n")
		  nil)
		)
	  )
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
	  (if (if-fill-constraints (car list) constraints t) ;; Looks for assigned person.
		  (car list) ;; Returns  selected person.
		(select-assigned (cdr list) constraints)
		)
	(progn
	  (princ "Nobody fills constraints: ")
	  (princ constraints)
	  (princ "\n")
	  (let ((offset (length Assignar)))
		;; Concat Assignar Germans.
		(princ "-->length of Assignar = ")
		(princ offset)
		(princ "\n")
		(princ "-->Concat Assignar Germans\n")
		(setq Assignar (append (append Assignar Germans) nil))
		(delete-dups Assignar)
		(select-assigned (nthcdr offset Assignar) constraints)
		 )
	  ;;nil
	  )
	);; TODO: append Germans. Returns nil if nobody fills constraints.
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
  (if (equal (car (car (cdr list))) "Single")
	  (progn
		(princ "Sigle: ")
		(print list)
		(princ "Name: ")
		(print (car (car list)))
		(setq Singles (append Singles (car list)))
		(set (intern (car (car list))) (cdr (cdr list)))
		(print "després\n")
		)
	(progn
	  (princ "Binary: ")
	  (print list)
	  (set (intern (car (car list))) (cdr list))
	  )
	)
  )

;;----------------------------------------------------------------------------;;
;; Creates list variables for all element in list.
;;
;; Calls decompose-list for all element in list.
;; Sets varibles of Restriccions list.
;;
;; Llistes
;; Sexe
;; Idioma
;; Primera-Visita
;; Primera-Revisita
;; Segona-Revisita
;; Tercera-Revisita
;; Curs-Biblic
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
;; Assign partner to select person.
;;
;;
;;----------------------------------------------------------------------------;;
(defun select-partner (person constraints partner-list nobody-fills)
  "Assign partner to select person."
  (if (not (null partner-list))
	  (let ((partner (car partner-list))) ;; Gets first person in partner list.
		(if (equal person partner) ;; Not allow to select same person as partner.
			(select-partner person constraints (cdr partner-list) nobody-fills) ;; Ignore same person.
		  (if (are-in-result Resultat person partner)
			  (select-partner person constraints (cdr partner-list) nobody-fills) ;; Person and Partner in result.
			(if (if-fill-constraints partner constraints nil) ;; Look for partner.
				(if (are-in-result Resultat partner person)
					(if (equal 14 (number-of-partners Resultat person 0))
						partner
					  (progn
						(princ "Exists asoc ")
						(princ partner)
						(princ " --> ")
						(princ person)
						(princ "\n")
						(select-partner person constraints (cdr partner-list) nobody-fills))
					  )
				  partner)
			  (select-partner person constraints (cdr partner-list) nobody-fills)
			  )
			)
		  )
		)
	(progn
	  (princ "Nobody fills ")
	  (princ person)
	  (princ " partner constraints: ")
	  (princ constraints)
	  (princ "\n")
	  (let ((offset (length Acompanyar)))
		;; Concat Acompanyar Germans.
		(princ "-->length of Acompanyar = ")
		(princ offset)
		(princ "\n")
		(princ "-->Concat Acompanyar Germans\n")
		(if (not nobody-fills)
		    (progn
		      (setq Acompanyar (append (append Acompanyar Germans) nil))
		      (delete-dups Acompanyar)
		      (select-partner person constraints (nthcdr offset Acompanyar) t)
		      )
		  (progn
			(princ "Atention!!! Need to change person: ")
			(princ person)
			(princ "\nBecause nobody can be his/her partner!!!")
			nil
			)
		  )
		)
	  ;;nil
	  )
	;;nil ;; Nobodies fills partner constraints. Or partner-list is empty.
	)
  )

;;----------------------------------------------------------------------------;;
;; Assign the given assignament to brother or sister.
;;
;; Called from assign-week (list).
;;
;; Variables set:
;;   Assignar
;;   Acompanyar
;;   Resultat
;;
;; Variables reset:
;;   Seleccio
;;----------------------------------------------------------------------------;;
(defun assign-assignement (list)
  "Assign the given assignament to brother or sister.

   Called from assign-week (list)."
  (if (boundp (intern (car list)))
	  (progn
		(print (car list))
		(let ((constraints (eval (intern (car list))))
				  (random-constraint)
				  (assigned) 
				  (partner))
		  (if (member (car list) Singles)
			  ;; Singles assigments like Lectura and Discurs.
			  (progn
				;; TODO: Fill this single assignement.
				(print "Single assignement")
				(princ "Assignar: ")
				(print Assignar)
				(princ "constraints: ")
				(print constraints)
				(setq assigned (select-assigned Assignar constraints))
;;				(if (is-in-result Resultat assigned);;TODO: NOT FINNISHED. ERROR...
					
				(setq assigned-this-week (append (list assigned) assigned-this-week)) ;; Adds people week.
				;; Debug
				(print "Week assigned people:")
				(print assigned-this-week)

				(setq Resultat (append Resultat (list (list (car list) (list assigned))))) ;;Adds person to result.
				(setq Assignar (delete assigned Assignar))    ;;Delete selected person to Assignar list.
				(princ "Assignar after delete: ")
				(print Assignar)
				(setq Seleccio nil) ;; Reset Seleccio global variable.
				)
			;; Else
			(progn
			  ;; Pass only 1 random constraint
			  (setq random-constraint (list (nth (random (length constraints)) constraints)))
			  (princ "Random constraint = ")
			  (princ random-constraint)
			  (princ "\n")
			  (setq assigned (select-assigned Assignar random-constraint))
			  (setq partner (select-partner assigned Seleccio Acompanyar nil)) ;; nil implies new search.
			  (if (null partner)
				  (progn
					(princ "++ Try with other person different from ")
					(princ assigned)
					(princ "\n")
					(setq Assignar (delete assigned Assignar)) ;; Deletes person imposible to assist.
					(setq Seleccio nil) ;; Reset Seleccio global variable.
					(assign-assignement list)
					)
				(progn
				  (setq Resultat (append Resultat (list (list (car list) (list assigned partner))))) ;;Adds person to result.
				  (setq assigned-this-week (append (list assigned) assigned-this-week)) ;; Adds people week.
				  (setq assigned-this-week (append (list partner) assigned-this-week)) ;; Adds people week.
				  ;; Debug
				  (print "Week assigned people:")
				  (print assigned-this-week)

				  (setq Assignar (delete assigned Assignar))    ;;Delete selected person to Assignar list.
				  (setq Acompanyar (delete partner Acompanyar)) ;;Delete selected partenr to Acompanyar list.
				  (setq Seleccio nil) ;; Reset Seleccio global variable.
				  )
				)
			  );; End else
			)
		  );; End if member Singles.
		);; End progn
	(progn
	  (princ "Not assign")
	  (princ list)
	  (princ "\n")
	  (setq Resultat (append Resultat (list list))) ;;Adds Data to result.
	  )
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
		(setq assigned-this-week nil) ;; Reset people assigned.
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

;;------------------------------------------------------------------------------
;; Writes output table header.
;;------------------------------------------------------------------------------
(defun output-table-header ()
  "Makes string with output table header."
  (concat "#+LATEX_CLASS: article\n"
	  "#+LATEX_CLASS_OPTIONS: [table]\n"
	  "#+OPTIONS: title:nil author:nil date:nil toc:nil\n"
	  "#+LATEX_HEADER: \\usepackage[margin=1in]{geometry}\n"
	  "#+LaTeX_HEADER: \\usepackage{booktabs}\n"
	  "#+LaTeX_HEADER: \\usepackage[table]{xcolor}\n"
	  "#+LaTeX_HEADER: \\usepackage{colortbl}\n"
	  "#+LATEX: \\definecolor{contiYellow}{RGB}{255,165,0}\n"
	  "#+LATEX: \\rowcolors[]{2}{contiYellow!5}{contiYellow!20}\n"
	  "#+ATTR_LATEX: :environment longtable\n"
	  "|Asignación|Asignado|Acompañante|")
  )

;;------------------------------------------------------------------------------
;; Writes string with assign info in table format.
;;------------------------------------------------------------------------------
(defun line-string-assigned (list)
  "Makes string with assign assigned and partner."
  (concat "|" (car list) "|" (car (car (car (car (cdr list)))))
		  "|" (car (car (car (cdr (car (cdr list)))))) "|")
  )

;;------------------------------------------------------------------------------
;; Writes string with assigns info in table format.
;;------------------------------------------------------------------------------
(defun make-full-output-assigneds (list)
  "Makes string with all assigns info."
  (if (not (null list))
	  (concat "\n" (line-string-assigned (car list)) (make-full-output-assigneds (cdr list)))
	)
  )

;;----------------------------------------------------------------------------;;
;; Saves output table.
;;----------------------------------------------------------------------------;;
(defun save-output-table (list month)
  (with-temp-buffer
	(insert (output-table-header))
	(insert (make-full-output-assigneds list))
	(write-file (concat "/home/gustavo/assignacions/output-" month ".org"))
	)
  (save-execution-results month) ;; Saves state tables.
  )

(provide 'licondefusr)

;;; licondefusr.el ends here
