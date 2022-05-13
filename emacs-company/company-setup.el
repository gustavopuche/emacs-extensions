(use-package company
  :ensure t
  :init
  (global-company-mode)
  :bind (("<tab>" . company-complete-common-or-cycle))
  :config)

(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

(provide 'company-setup)
