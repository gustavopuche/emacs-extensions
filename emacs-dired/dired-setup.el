(defun dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://xahlee.info/emacs/emacs/dired_sort.html'
Version: 2018-12-23 2022-04-07"
  (interactive)
  (let (xsortBy xarg)
    (setq xsortBy (completing-read "Sort by:" '( "date" "size" "name" )))
    (cond
     ((equal xsortBy "name") (setq xarg "-Al "))
     ((equal xsortBy "date") (setq xarg "-Al -t"))
     ((equal xsortBy "size") (setq xarg "-Al -S"))
     ((equal xsortBy "dir") (setq xarg "-Al --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other xarg )))

(setq dired-guess-shell-alist-user '(("\\.pdf\\'" "evince")
                                   ("\\.doc\\'" "libreoffice")
                                   ("\\.docx\\'" "libreoffice")
                                   ("\\.ppt\\'" "libreoffice")
                                   ("\\.pptx\\'" "libreoffice")
                                   ("\\.xls\\'" "libreoffice")
                                   ("\\.xlsx\\'" "libreoffice")))

(require 'openwith)
(setq openwith-associations
      (list
       (list (openwith-make-extension-regexp
	      '("doc" "docx" "ppt" "pttx" "xls" "xlsx" "odt" "odf"))
	     "soffice"
	     '(file))))
(openwith-mode 1)

(setq dired-omit-files
    (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
        (seq bol "." (not (any "."))) ;; dot-files
        (seq "~" eol)                 ;; backup-files
        (seq bol "CVS" eol)           ;; CVS dirs
        )))

(provide 'dired-setup)
