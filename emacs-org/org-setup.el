(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

;; (setq org-odt-styles-file "/home/gustavo/org/styles.odt")
;; (require 'ox-org)
(require 'org-brain)
(require 'calfw-cal)
(require 'calfw-ical)
(require 'calfw-org)

(defun refresh-org-agenda-files ()
   (interactive)
   (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$")))

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
(require 'ob-dot)

(provide 'org-setup)
