(use-package treemacs-all-the-icons  :ensure t)
(use-package powerline :ensure t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Enable customized theme
  ;; FIXME https://github.com/emacs-lsp/lsp-treemacs/issues/89
  ;; (with-eval-after-load 'lsp-treemacs
  ;;   (setq treemacs-no-png-images t))
  )

(with-eval-after-load 'doom-themes
  (load-theme 'infodoc t)

  ;; Use variable width font faces in current buffer
  (defun programming-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "MSX Screen 0" :foundry "PfEd" :slant normal :weight normal :height 75 :width normal))
    (buffer-face-mode))
  
  (defun org-brain-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 90 :width normal))
    (buffer-face-mode))

  (defun treemacs-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 85 :width normal))
    (buffer-face-mode))

  (defun dired-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "CPMono_v07" :foundry "PYRS" :slant normal :weight extralight :height 98 :width normal))
    (buffer-face-mode))

  (add-hook 'treemacs-mode-hook 'treemacs-face-mode)
  (add-hook 'dired-mode-hook 'dired-face-mode)
  (add-hook 'org-brain-visualize-mode-hook 'org-brain-face-mode)
  (add-hook 'org-mode-hook 'org-brain-face-mode)
  (add-hook 'c++-mode-hook 'dired-face-mode)
  (add-hook 'c-mode-hook 'dired-face-mode)
  (add-hook 'emacs-lisp-mode-hook 'dired-face-mode)
  [add-hook 'text-mode-hook 'dired-face-mode]
  (add-hook 'which-key-mode-hook 'dired-face-mode)
  (add-hook 'helm-mode-hook 'dired-face-mode)
  )

(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "CPMono_v07" :foundry "PYRS" :slant normal :weight extralight :height 98 :width normal)))))

(provide 'themes-setup)
;;
