(require 'org)
(require 'org-brain)

(require 'ol-man) ;; Allows man links. e.g. [[man:printf][Printf Manual Page]]
(require 'org-extra-emphasis)
;; (use-package ox-odt :ensure t)
;; (use-package org-kanban :ensure t)
(require 'org-indent)

(use-package org-roam :ensure t)
(use-package org-roam-ui :ensure t)
(setq org-roam-directory (file-truename "~/org-roam"))
(org-roam-db-autosync-mode) ;; performance cost
(org-roam-db-autosync-mode)

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

