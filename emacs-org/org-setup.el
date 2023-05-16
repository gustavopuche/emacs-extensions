(require 'org)
(require 'org-brain)
;; (use-package calfw-cal :ensure t)
;; (use-package calfw-ical :ensure t)
;; (use-package calfw-org :ensure t)
(require 'org-extra-emphasis)
(use-package ox-odt :ensure t)
;; (use-package ob-dot :ensure t)

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

;; (setq org-odt-styles-file "/home/gustavo/org/styles.odt")
;; (require 'ox-org)

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
		(org-agenda-span 'year)
                (org-agenda-start-with-log-mode '(closed))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\* DONE ")))))

;; visual-line-mode active with latex-mode
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-brain-visualize-text-hook (lambda () (visual-line-mode 1)
					   (org-display-inline-images)))


;; this controls the color of bold, italic, underline, verbatim, strikethrough
(setq org-emphasis-alist
  '(("*" (bold :foreground "orange" )) ;; this make bold both italic and bold, but not color change
    ("/" (italic :foreground "dark salmon" )) ;; italic text, the text will be "dark salmon"
    ("_" (italic :foreground "DarkViolet") ) ;; underlined text, color is "DarkViolet"
    ("=" (bold :foreground "DeepSkyBlue" )) 
    ("~" (bold :foreground "MediumSeaGreen" ))
    ("+" (bold :foreground "tomato" ))))
(setq org-hide-emphasis-markers t) ;; hides the emphasis markers

;; Add organizer.org file to agenda.
(org-agenda-files '("~/org/brain/organizer.org"))

(setq org-todo-keywords '((sequence "TODO(t)" "DOING" "BLOCKED(b@/!)" "REVIEW" "|" "DONE(d!)" "CANCELLED(c@)")))

(setq org-log-done 'time)
(setq org-todo-keyword-faces '(("todo" . "SlateGray")
			       ("doing" . "DarkOrchid")
			       ("blocked" . "Firebrick")
			       ("done" . "ForestGreen")
			       ("cancelled" .  "SlateBlue")))

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

(provide 'org-setup)
