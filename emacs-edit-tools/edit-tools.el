;;; edit-tools.el --- Special edit operatios,

;; Copiright (C) 2021 Gustavo Puche

;; Author: Gustavo Puche <gustavo.puche@gmail.com>
;; Created: 15 December 2021
;; Version: 0.1
;; Keywords: languages all
;; Package-Requires: 
(use-package drag-stuff :ensure t)
(use-package ws-butler :delight :ensure t)
(require 'highlight-global)

;; Solve problem with dead keys.
(require 'iso-transl)

;;; Code:
(defconst edit-tools--debug-comments "^.*//.*Debug\.*\n.*\n.*//////.*\n")
(defconst edit-tools--empty-line-with-spaces "^[[:space:]]+$")

(defvar edit-tools--fdfind "")
(defvar edit-tools--find-cmd "")

(defun edit-tools-regex-global-replace (regex1 regex2)
 "Replace `regex1' with `regex2' in a buffer."
 (goto-char 1)
 (while (search-forward-regexp regex1 nil)
 (replace-match regex2)))

(defun edit-tools-remove-whitespaces-in-empty-lines ()
  "Remove all whitespaces in empty lines."
  (interactive)
  (edit-tools-regex-global-replace edit-tools--empty-line-with-spaces ""))

(defun edit-tools-remove-debug-comments ()
  "Clean debug comments."
  (interactive)
  (edit-tools-regex-global-replace edit-tools--debug-comments ""))

(defun edit-tools-hack-isearch-kill ()
   "Push current matching string into kill ring."
   (interactive)
   (kill-new (buffer-substring (point) isearch-other-end))
   (isearch-done))

(defun edit-tools-insert-include ()
  (interactive)
  (let ((include-str))
    (with-current-buffer
	(progn
	  (setq include-str (edit-tools-get-include
			     (concat
			      (buffer-substring-no-properties
			       (region-beginning) (region-end))
			      ".h")))
	  (message include-str)
	  (deactivate-mark)
	  (search-backward "#include")
	  (line-move 1)
	  (insert (concat "#include \"" include-str "\""))
	  (newline)))))

(defun edit-tools-get-include (filename)
  (replace-regexp-in-string
   (projectile-project-root) ""
   (replace-regexp-in-string
    "\n$" "" (edit-tools-set-fdfind filename))))

(defun edit-tools-set-fdfind (filename)
  (setq edit-tools--fdfind (shell-command-to-string (concat "fdfind --color=never -g " filename " " (projectile-project-root)))))

(defun edit-tools-find-cmd (filename)
  (setq edit-tools--find-cmd (concat "fdfind --color=never -g " filename " " (projectile-project-root))))

(defun swap-buffer ()
  "Swap buffers order."
  (interactive)
  (cond ((one-window-p) (display-buffer (other-buffer)))
        ((let* ((buffer-a (current-buffer))
                (window-b (cadr (window-list)))
                (buffer-b (window-buffer window-b)))
           (set-window-buffer window-b buffer-a)
           (switch-to-buffer buffer-b)
           (other-window 1)))))

(defun reload-init-file ()
  (interactive)
  (load-file "~/.emacs"))

(defun matches-in-buffer ()
  "return a list of matches of REGEXP in BUFFER or the current buffer if not given."
  (interactive)
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (current-buffer)
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp "[A-Z0-9_]+\\.\\(DLL\\|OCX\\)" nil t 1)
              (push (concat (match-string 0) "\n") matches))
			)
		  )
		)
	  matches)
	(kill-new (string-join (reverse matches)))
	)
  )

(defun fun-temp ()
  (interactive)
  (let ((matches))
	(add-to-ordered-list 'matches "hola\n")
	(add-to-ordered-list 'matches "como\n" )
	(push "estas\n" matches)
	(kill-new (string-join (reverse matches)))
	))

(defun fun-buffer ()
  (interactive)
  (with-temp-buffer (insert "Aço es una proba")) (clipboard-kill-region (point-min) (point-max)))

(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)


;; Delete selected text when a key is pressed.
(delete-selection-mode 1)

;; Refresh buffers when changed by other program.
(auto-revert-mode 1)

;; Avoid 'file changed on disk' message.
(setq revert-without-query '(".*"))

;; enable drag-stuff everywhere
(drag-stuff-global-mode t)

;; display column-number in mode line.
(column-number-mode t)

;; apply default key bindings
(drag-stuff-define-keys)

(global-set-key (kbd "M-H") 'highlight-frame-toggle)


;; Dired 2 panels.
(setq dired-dwim-target t)

;; Hide dired hidden files.
(setq dired-listing-switches "-la")

;; Hide dired details.
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; ;; Copy org tags into other file.
;; (let* ((target-heading "Destination")
;;        (target-marker (org-find-exact-headline-in-buffer target-heading))
;;        (target-id (save-excursion (goto-char target-marker)
;;                                   (org-id-get-create))))
;;   target-id)
;; ;; => "22bdee5f-3c93-4ca7-b944-48db2ddc3538"

;; (defun ebpa/refile-matches-to-heading (match target-heading-id &optional scope copy)
;;   "Refile all headings within SCOPE (per `org-map-entries') to TARGET-HEADING-ID."
;;   (if-let (target-marker (org-id-find target-heading-id t))
;;       (let* ((target-rfloc (with-current-buffer (marker-buffer target-marker)
;;                              (goto-char target-marker)
;;                              (list (org-get-heading)
;;                                    (buffer-file-name (marker-buffer target-marker))
;;                                    nil
;;                                    target-marker)))
;;              (headings-to-copy (org-map-entries (lambda () (point-marker)) match scope)))
;;         (mapc
;;          (lambda (heading-marker)
;;            (with-current-buffer (marker-buffer heading-marker)
;;              (goto-char heading-marker)
;;              (org-refile nil nil target-rfloc (when copy "Copy"))))
;;          headings-to-copy)
;;         (message "%s %d headings!"
;;                  (if copy "Copied" "Refiled")
;;                  (length headings-to-copy)))
;;     (warn "Could not find target heading %S" target-heading-id)))

;; You can use it in the following way:

;; (ebpa/refile-matches-to-heading
;;  "TAGS={@Tag}"
;;  "22bdee5f-3c93-4ca7-b944-48db2ddc3538"
;;  'file
;;  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun highlight-dll-ocx ()
;;   (interactive)
;;   (highlight-regexp "[A-Z0-9_]+\\.\\(DLL\\|OCX\\)"))

;;(global-set-key (kbd "<f4>") 'highlight-dll-ocx)

;; (defun matches-in-buffer ()
;;   "return a list of matches of REGEXP in BUFFER or the current buffer if not given."
;;   (interactive)
;;   (let ((matches))
;;     (save-match-data
;;       (save-excursion
;;         (with-current-buffer (current-buffer)
;;           (save-restriction
;;             (widen)
;;             (goto-char 1)
;;             (while (search-forward-regexp "[A-Z0-9_]+\\.\\(DLL\\|OCX\\)" nil t 1)
;;               (push (concat (match-string 0) "\n") matches))
;; 			)
;; 		  )
;; 		)
;; 	  matches)
;; 	(kill-new (string-join (reverse matches)))
;; 	)
;;   )

;; (defun fun-temp ()
;;   (interactive)
;;   (let ((matches))
;; 	(add-to-ordered-list 'matches "hola\n")
;; 	(add-to-ordered-list 'matches "como\n" )
;; 	(push "estas\n" matches)
;; 	(kill-new (string-join (reverse matches)))
;; 	))

;; (defun fun-buffer ()
;;   (interactive)
;;   (with-temp-buffer (insert "Aço es una proba")) (clipboard-kill-region (point-min) (point-max)))

;; (global-set-key (kbd "<f5>") 'matches-in-buffer)
;; (global-set-key (kbd "<f6>") 'fun-temp)
;; (global-set-key (kbd "<f7>") 'fun-buffer)

(provide 'edit-tools)
