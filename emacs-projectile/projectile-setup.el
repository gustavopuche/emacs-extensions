;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name))))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq helm-projectile-fuzzy-match nil)
(helm-projectile-on)

;; Tramp stuff.
(setq tramp-save-ad-hoc-proxies t)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(provide 'projectile-setup)
