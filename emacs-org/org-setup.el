
(require 'org)
(require 'org-brain)

(require 'ol-man) ;; Allows man links. e.g. [[man:printf][Printf Manual Page]]
(require 'org-extra-emphasis)
;; (use-package ox-odt :ensure t)
;; (use-package org-kanban :ensure t)
(require 'org-indent)

(if (version< "29.3" emacs-version)
    (progn
      (use-package org-roam :ensure t)
      (use-package org-roam-ui :ensure t)
      (setq org-roam-directory (file-truename "~/org-roam"))
      (org-roam-db-autosync-mode) ;; performance cost
      (org-roam-db-autosync-mode)))

(add-to-list 'display-buffer-alist
                  '("\\*org-roam\\*"
                    (display-buffer-in-side-window)
                    (side . right)
                    (slot . 0)
                    (window-width . 0.33)
                    (window-parameters . ((no-other-window . t)
                                          (no-delete-other-windows . t)))))

(use-package org-sliced-images
  :ensure t
  :config
  (org-sliced-images-mode 1))

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

;;;;;;;;;;;;
;; Agenda
;;;;;;;;;;;;

;; Functions
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

;; Agenda files
(defun refresh-org-agenda-files ()
   (interactive)
   (setq org-agenda-files (directory-files-recursively "~/org-roam" "\\.org$"))
   (setq org-agenda-prefix-format
      '((agenda . " %i %?-12t% s")
        (todo . " %i ")
        (tags . " %i ")
        (search . " %i "))))

(refresh-org-agenda-files)

;; Custom commands
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
	     '("D" "All Done Tasks"
               todo "DONE"
               ((org-agenda-overriding-header "Completed Tasks Collection"))))

(add-to-list 'org-agenda-custom-commands
	     '("B" "All Blocked Tasks"
               todo "BLOCKED"
               ((org-agenda-overriding-header "Blocked Tasks Collection"))))

(add-to-list 'org-agenda-custom-commands
	     '("d" "Daily agenda and all TODOs"
		((tags "PRIORITY=\"A\""
                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			(org-agenda-overriding-header "High-priority unfinished tasks:")))
		 (agenda "" ((org-agenda-ndays 1)))
		 (alltodo ""
			  ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
							  (air-org-skip-subtree-if-priority ?A)
							  (org-agenda-skip-if nil '(scheduled deadline))))
			   (org-agenda-overriding-header "ALL normal priority tasks:"))))
		((org-agenda-compact-blocks t))))

;; visual-line-mode active with latex-mode
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-brain-visualize-text-hook (lambda () (visual-line-mode 1)
					   (org-display-inline-images)
					   (setq tab-width 8)))
(add-hook 'org-mode-hook (lambda () 
			   (setq tab-width 8)))

;; Add organizer.org file to agenda.
(org-agenda-files '("~/org/brain/organizer.org"))

(setq org-todo-keywords '((sequence "TODO(t!)" "DOING(o!)" "BLOCKED(b@)" "REVIEW(r!)" "BUG(b!)" "|" "DONE(d!)" "CANCELLED(c@)" "FIXED(f!)")))

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

(defun capture-tag-entries (tag cut)
  "Extract all entries and children tagged with :debug from the current file 
and append them to debug.org."
  (interactive)
  (let ((entries '()))
    ;; Step 1: Collect the text of all matching subtrees
    (org-map-entries
     (lambda ()
       (push (org-copy-subtree nil cut nil nil) entries)
       (setq org-map-continue-from (point-min)))
     tag 'file)
    (string-join (reverse entries))
))
(setq org-directory "~/org-roam")
(setq org-capture-templates
      '(("d" "Extract current :debug: entries" entry 
         (file "debug.org")  ;; <-- Define your target file here!
         (function (lambda () (capture-tag-entries "debug" nil)))
         :immediate-finish t)
	("c" "Extract current :cpp: entries" entry 
         (file "cpp.org")  ;; <-- Define your target file here!
         (function (lambda () (capture-tag-entries "cpp" nil)))
         :immediate-finish t)
	("b" "Extract current :bug: entries" entry 
         (file "bugs.org")  ;; <-- Define your target file here!
         (function (lambda () (capture-tag-entries "bug" nil)))
         :immediate-finish t)
	("p" "Prompt us for input" entry
	 (file+headline "demo.org" "Our first heading")
	 "* %^{Please write here} %?")
	("o" "Options in prompt" entry
	 (file+headline "demo.org" "Our first heading")
	 "* %^{Select your option|ONE|TWO|THREE} %?")
	("t" "Task with a Date" entry
	 (file+headline "demo.org" "Scheduled tasks")
	 "* %^{Select your option|ONE|TWO|THREE}\n SCHEDULED: %t\n %i %?")
	("i" "Task with a Date" entry
	 (file+headline "ideas.org" "Ideas to do")
	 "* %^{Select your option|Work|Home|Service}\n SCHEDULED: %t\n %i %?")
	("a" "Task with ask for a Date" entry
	 (file+headline "demo.org" "Scheduled tasks")
	 "* %^{Select your option|ONE|TWO|THREE}\n SCHEDULED: %^t\n %i %?")
	("r" "Remove tags")
	("rb" "Remove current :bug: entries" entry 
         (file "bugs.org")  ;; <-- Define your target file here!
         (function (lambda () (capture-tag-entries "bug" t)))
	 :immediate-finish t)
	("rp" "Prompt us for input" entry
	 (file+headline "demo.org" "Our first heading")
	 "* %^{Please write here} %?")))

(setq org-startup-with-inline-images t)
(setq org-support-shift-select t)

;; To resize images with #+ATTR_ORG: :width 600
(setq org-image-actual-width nil)

;; Use graphviz inside org.

;; org config.
(custom-set-variables
 '(org-log-into-drawer t)
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
             '("koma-article"
               "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defun org-brain-insert-resource-icon (link)
  "Insert an icon, based on content of org-mode LINK."
  (insert (format "%s "
                  (cond ((string-prefix-p "brain:" link)
                         (all-the-icons-fileicon "brain"))
                        ((string-prefix-p "info:" link)
                         (all-the-icons-octicon "info"))
                        ((string-prefix-p "help:" link)
                         (all-the-icons-material "help"))
                        ((string-prefix-p "http" link)
                         (all-the-icons-icon-for-url link))
                        (t
                         (all-the-icons-icon-for-file link))))))

(defun org-open-other-frame ()
  "Jump to bookmark in another frame. See `bookmark-jump' for more."
  (interactive)
  (let ((org-link-frame-setup (acons 'file 'find-file-other-frame org-link-frame-setup)))
    (org-open-at-point)))

(add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)

(provide 'org-setup)
