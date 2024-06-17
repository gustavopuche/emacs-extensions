;;;;;;;;;;;;;;;;;;;
;; LSP Mode      ;;
;;;;;;;;;;;;;;;;;;;
(use-package irony :delight :ensure t)
(use-package dap-mode :delight :ensure t)
(use-package yasnippet :delight yas-minor-mode :ensure t)
(use-package flycheck :delight :ensure t)
(use-package flycheck-irony :delight :ensure t)
(use-package drag-stuff :delight :ensure t)
(use-package which-key :delight :ensure t)
(use-package lsp-mode :ensure t)
(use-package lsp-ui :ensure t)

;; Enable yasnippet
(yas-global-mode 1)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'lsp-mode-hook (lambda()(toggle-truncate-lines 1)))
  (require 'dap-cpptools)
  (require 'helm-lsp)
  (yas-global-mode)
  (setq lsp-ui-doc-enable nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\Q5\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\W2\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.o\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.d\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.dt\\'")
  )

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'sgml-mode-hook #'lsp-deferred)
(add-hook 'sh-mode-hook 'lsp)
(add-hook 'perl-mode-hook 'lsp)
(add-hook 'js-mode-hook 'lsp)

(setq lsp-clangd-binary-path "/usr/bin/clangd")

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;;------------------------------------------------------------------------------
;; Auto insertion of headers
(autoload 'cpp-auto-include/namespace-qualify-file "cpp-auto-include"
  "Explicitly qualify uses of the standard library with their namespace(s)." t)
(autoload 'cpp-auto-include/ensure-includes-for-file "cpp-auto-include"
  "Auto-insert #include line(s) required for the current buffer." t)
(autoload 'cpp-auto-include/ensure-includes-for-current-line "cpp-auto-include"
  "Auto-insert #include line(s) required for the current line." t)
(eval-after-load 'cc-mode
  '(bind-keys :map c++-mode-map
              ("C-c q" . cpp-auto-include/namespace-qualify-file)
              ("C-c i" . cpp-auto-include/ensure-includes-for-file)
              ("C-c o" . cpp-auto-include/ensure-includes-for-current-line)))

;;------------------------------------------------------------------------------

;; Fixes isearch mode cancelled when moving mouse.
(put 'dap-tooltip-mouse-motion 'isearch-scroll t)
(put 'handle-switch-frame 'isearch-scroll t)

(setq lsp-ui-doc-enable nil
      lsp-ui-sideline-enable nil)

;; Disable incons in header line.
(setq lsp-headerline-breadcrumb-icons-enable nil)

(provide 'lsp-setup)
