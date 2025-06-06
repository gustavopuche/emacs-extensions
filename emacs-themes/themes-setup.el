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
  (load-theme 'gus-light-blue t)

  ;; Use variable width font faces in current buffer
  (defun msx-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "MSX Screen 0" :foundry "PfEd" :slant normal :weight normal :height 75 :width normal))
    (buffer-face-mode))

  (defun programming-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "CPMono_v07 Bold" :weight light :height 115))
    (buffer-face-mode))

  (defun bold-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "CPMono_v07 Bold" :weight normal :height 95))
    (buffer-face-mode))

  (defun org-brain-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 125 :width normal))
    (buffer-face-mode))

  (defun treemacs-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 115 :width normal))
    (buffer-face-mode))

  (defun dired-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "CPMono_v07" :weight light :height 98))
    (buffer-face-mode))

  (defun extralight-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "CPMono_v07 ExtraLight" :height 98))
    (buffer-face-mode))

  (defun light-face-mode ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "CPMono_v07 Light" :height 98))
    (buffer-face-mode))

  (add-hook 'treemacs-mode-hook (lambda()
				  (setq treemacs-show-hidden-files nil)
				  (setq treemacs-max-git-entries 10000)
				  (treemacs-fringe-indicator-mode -1)
				  (treemacs-face-mode)
				  (setq treemacs-collapse-dirs 0)
				  (treemacs-git-mode -1)))
  
  (add-hook 'dired-mode-hook 'dired-face-mode)
  (add-hook 'org-brain-visualize-mode-hook 'org-brain-face-mode)
  (add-hook 'org-mode-hook 'org-brain-face-mode)
  (add-hook 'c++-mode-hook 'programming-face-mode)
  (add-hook 'c-mode-hook 'programming-face-mode)
  (add-hook 'emacs-lisp-mode-hook 'bold-face-mode)
  [add-hook 'text-mode-hook 'dired-face-mode]
  (add-hook 'which-key-mode-hook 'dired-face-mode)
  (add-hook 'helm-mode-hook 'dired-face-mode)

  (set-face-attribute 'default nil :font "CPMono_v07 Bold" :height 98)
  )

(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(provide 'themes-setup)
;;
