(use-package company
  :delight
  :ensure t
  :init
  (global-company-mode)
  :bind (("s-<space>" . company-complete-common-or-cycle))
  :config)

(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

(provide 'company-setup)
