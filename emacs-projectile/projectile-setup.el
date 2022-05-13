;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq helm-projectile-fuzzy-match nil)
(helm-projectile-on)

(setq projectile-generic-command "fd . -0 --type f --color=never -e cpp -e h -e el")
(setq projectile-git-command "fd . -0 --type f --color=never -e cpp -e h -e el")

(provide 'projectile-setup)
