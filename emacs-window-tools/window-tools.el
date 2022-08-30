;;; window-tools.el --- Common operatios with emacs windos.

;; Copiright (C) 2020 Gustavo Puche

;; Author: Gustavo Puche <gustavo.puche@gmail.com>
;; Created: 18 June 2020
;; Version: 0.1
;; Keywords: languages all
;; Package-Requires: 

;;; Code:
(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (shrink-window (window-height))
	(enlarge-window 8))


(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun swap-buffer ()
  (interactive)
  (cond ((one-window-p) (display-buffer (other-buffer)))
        ((let* ((buffer-a (current-buffer))
                (window-b (cadr (window-list)))
                (buffer-b (window-buffer window-b)))
           (set-window-buffer window-b buffer-a)
           (switch-to-buffer buffer-b)
           (other-window 1)))))

(global-set-key (kbd "C-c s") 'swap-buffer)
(global-set-key (kbd "C-c v") 'halve-other-window-height)
(global-set-key (kbd "<M-s-down>") 'shrink-window)
(global-set-key (kbd "<M-s-up>") 'enlarge-window)
(global-set-key (kbd "<C-M-s-up>") 'window-split-toggle)
(global-set-key (kbd "<C-M-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-M-right>") 'enlarge-window-horizontally)

;; delight
(delight 'doxy-graph-mode nil)
(delight 'abbrev-mode nil 'abbrev)
(delight 'eldoc-mode nil "eldoc")

;; Power line
(powerline-default-theme)

(provide 'window-tools)
