(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") 

;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "MSX Screen 0" :foundry "PfEd" :slant normal :weight normal :height 98 :width normal)))))


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
  (setq doom-variable-pitch-font (font-spec :family "Ubuntu Mono" :foundry "DAMA" :size 10))
  (doom-themes-org-config)

  ;; Enable customized theme
  ;; FIXME https://github.com/emacs-lsp/lsp-treemacs/issues/89
  ;; (with-eval-after-load 'lsp-treemacs
  ;;   (setq treemacs-no-png-images t))
  )

 ;; Use variable width font faces in current buffer
(defun treemacs-face-mode ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 100 :width normal))
  (buffer-face-mode))

(defun org-brain-face-mode ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 130 :width normal))
  (buffer-face-mode))


(add-hook 'treemacs-mode-hook 'treemacs-face-mode)
(add-hook 'org-brain-visualize-mode-hook 'org-brain-face-mode)
(add-hook 'org-mode-hook 'org-brain-face-mode)

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 143 :width normal)))))


(provide 'themes-setup)

;; 
