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

(setq projectile-generic-command "fdfind . -0 --type f --color=never -e cpp -e h-e cc -e el -e ini -e xml -e tex -e pl -e pm")
(setq projectile-git-command "fdfind . -0 --type f --color=never -e cpp -e h -e cc -e el -e ini -e xml -e tex -e pl -e pm")

(provide 'projectile-setup)
