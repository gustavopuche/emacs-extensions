(require 'org)
(require 'org-brain)

(require 'org-extra-emphasis)
(use-package ox-odt :ensure t)
(use-package org-kanban :ensure t)

(setq org-hide-emphasis-markers t)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot

(setq org-log-done t)

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(defun refresh-org-agenda-files ()
   (interactive)
   (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$")))

(refresh-org-agenda-files)
(add-to-list 'org-agenda-custom-commands
             '("W" "Closed tasks previous weeks"
               agenda ""
               ((org-agenda-start-day "-14d")
                (org-agenda-span 14)
                (org-agenda-start-on-weekday 1)
                (org-agenda-start-with-log-mode '(closed))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\* DONE ")))))

(add-to-list 'org-agenda-custom-commands
	     '("w" "Previous weeks all"
               agenda ""
               ((org-agenda-start-day "-14d")
                (org-agenda-span 14)
                (org-agenda-start-on-weekday 1))))

(add-to-list 'org-agenda-custom-commands
	     '("D" "Show all DONE tasks"
               agenda ""
               ((org-agenda-start-day "2022-01-01")
		(org-agenda-span 1000)
                (org-agenda-start-with-log-mode '(closed))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\* DONE ")))))

;; visual-line-mode active with latex-mode
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-brain-visualize-text-hook (lambda () (visual-line-mode 1)
					   (org-display-inline-images)
					   (setq tab-width 2)))
(add-hook 'org-mode-hook (lambda () 
			   (setq tab-width 2)))



;; Add organizer.org file to agenda.
(org-agenda-files '("~/org/brain/organizer.org"))

(setq org-todo-keywords '((sequence "TODO(t)" "DOING(o)" "BLOCKED(b@)" "REVIEW(r)" "BUG(b)" "|" "DONE(d!)" "CANCELLED(c@)" "FIXED(f!)")))

(setq org-log-done 'time)
(setq org-todo-keyword-faces '(("todo" . "SlateGray")
			       ("doing" . "DarkOrchid")
			       ("blocked" . "Firebrick")
			       ("bug" . "Green")
			       ("done" . "ForestGreen")
			       ("cancelled" .  "SlateBlue")
			       ("fixed" . "Yellow")))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-startup-with-inline-images t)
(setq org-support-shift-select t)

;; To resize images with #+ATTR_ORG: :width 600
(setq org-image-actual-width nil)

;; Use graphviz inside org.

;; ox-odt config.
(custom-set-variables
 '(org-odt-convert-process "LibreOffice")
 '(org-odt-preferred-output-format "docx")
 '(org-odt-transform-processes
   '(("Optimize Column Width of all Tables"
      "soffice" "--norestore" "--invisible" "--headless"
      "macro:///OrgMode.Utilities.OptimizeColumnWidth(%I)")
     ("Update All"
      "soffice" "--norestore" "--invisible" "--headless"
      "macro:///OrgMode.Utilities.UpdateAll(%I)")
     ("Reload"
      "soffice" "--norestore" "--invisible" "--headless"
      "macro:///OrgMode.Utilities.Reload(%I)")))
 '(org-latex-to-mathml-convert-command
   "java -jar %j -unicode -force -df %o %I")
 '(org-latex-to-mathml-jar-file
   "/home/kjambunathan/Downloads/mathtoweb.jar"))

;; (setcdr (assq 'system org-file-apps-defaults-gnu) "xdg-open %s")
(setcdr (assq 'system org-file-apps-gnu) "xdg-open %s")

(advice-add 'org-open-file :around
            (lambda (orig-fun &rest args)
              ;; Work around a weird problem with xdg-open.
              (let ((process-connection-type nil))
                (apply orig-fun args))))

;; org latex classes
(add-to-list 'org-latex-classes
<<<<<<< HEAD
           '("koma-article"
              "\\documentclass{scrartcl}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
=======
          '("koma-article"
             "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
>>>>>>> 24930fb (Some minor changes)

(provide 'org-setup)

;; To use with doom-one-light theme

(custom-set-faces
 '(org-default ((t ( 
                    :inherit default
                    ))))
 '(org-extra-emphasis ((t ( 
                           :height 2.0
                           :inherit default
                           :weight black
                           :width normal
                           ))))
 '(org-extra-emphasis-01 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "#A53E2D"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-02 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "ForestGreen"
                              :inherit org-extra-emphasis
                              :weight bold
                              ))))
 '(org-extra-emphasis-03 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "gold2"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-04 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "cyan3"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-05 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "#54436E"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-06 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "#D65D8F"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-07 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "#E5C494"
                              :inherit org-extra-emphasis
                              :weight extra-bold
                              ))))
 '(org-extra-emphasis-08 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "DarkOrange1"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-09 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "firebrick1"
                              :inherit org-extra-emphasis
                              ))))
  '(org-extra-emphasis-10 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "SpringGreen3"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-11 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "yellow3"
                              :inherit org-extra-emphasis
                              :weight bold
                              ))))
 '(org-extra-emphasis-12 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "DeepSkyBlue2"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-13 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "magenta4"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-14 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "burlywood4"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-15 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "sienna2"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-16 ((t ( 
                              :family "EnhancedDotDigital-7"
                              :foreground "SlateBlue3"
                              :inherit org-extra-emphasis
                              :weight extra-bold
                              )))))
