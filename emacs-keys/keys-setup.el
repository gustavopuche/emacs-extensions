;;;;;;;;;;;;;;;;;;;
;; Keybinds      ;;
;;;;;;;;;;;;;;;;;;;

;; Define keys
(setq lsp-keymap-prefix "C-s-l")
(global-set-key [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
(global-set-key (kbd "<f1>") 'which-key-show-top-level)
(global-set-key (kbd "<f2>") 'helm-occur)
(global-set-key (kbd "<f3>") 'helm-gtags-find-tag-from-here)
(global-set-key (kbd "<f4>") 'helm-imenu)

(global-set-key (kbd "<S-f1>") 'treemacs)
(global-set-key (kbd "<S-f2>") 'woman)

(defun set-compile-keys ()
  (interactive)
  (global-set-key (kbd "<f5>") 'compile-tools-compile-make-run)
  (global-set-key (kbd "<f6>") 'compile-tools-debug)
  (global-set-key (kbd "<f7>") 'compile-tools-run-gtest)
  (global-set-key (kbd "<f8>") 'compile-tools-compile-cmake)
  (global-set-key (kbd "<f9>") 'compile-tools-compile-make))

(with-eval-after-load 'cmake-mode
  (set-compile-keys))

(with-eval-after-load 'c++-mode
  (set-compile-keys))

(global-set-key (kbd "<backtab>") 'other-window)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-window-any-frame)
(global-set-key (kbd "s-SPC") 'company-capf)
(global-set-key (kbd "<C-f2>") 'compile-tools-set-qt-build-path)
(global-set-key (kbd "<C-f3>") 'helm-gtags-pop-stack)
(global-set-key (kbd "<C-f4>") 'vc-annotate)
(global-set-key (kbd "<C-f8>") 'compile-tools-reset-target)
(global-set-key (kbd "<C-f9>") 'compile-tools-run-perl-sics-install)
(global-set-key (kbd "<C-f10>") 'compile-tools-compile-make-install)
(global-set-key (kbd "<C-f12>") 'kill-this-buffer)
(global-set-key (kbd "M-s-<right>") 'next-buffer)
(global-set-key (kbd "M-s-<left>") 'previous-buffer)

(global-set-key (kbd "<S-f12>") 'lsp-ui-peek-find-references)

(global-set-key (kbd "<C-S-f10>") 'lsp-ui-peek-find-definitions)

(global-set-key (kbd "<C-s-f9>") 'compile-tools-compile-make-aab)
(global-set-key (kbd "<C-s-f4>") 'magit-blame)
(global-set-key (kbd "<C-s-f2>") 'compile-tools-set-target)
(global-set-key (kbd "<C-S-f3>") 'xref-find-definitions-other-window)

(global-set-key (kbd "C-c s") 'swap-buffer)
(global-set-key (kbd "C-c C-l") 'reload-init-file)
(define-key isearch-mode-map (kbd "M-w") 'edit-tools-hack-isearch-kill)

;; Bind date keybindding
(global-set-key (kbd "C-c C-d") "\C-u\M-! date -I")

;; Select All keybinding
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-s-u") 'upcase-char)


(global-set-key (kbd "M-t") 'helm-gtags-find-tag)
(global-set-key (kbd "M-r") 'helm-gtags-find-rtag)
(global-set-key (kbd "M-s") 'helm-gtags-find-symbol)
(global-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)
(global-set-key (kbd "C-c <") 'helm-gtags-previous-history)
(global-set-key (kbd "C-c >") 'helm-gtags-next-history)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)

(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

(define-key global-map (kbd "M-,") 'xref-pop-marker-stack)
(define-key global-map [remap list-buffers] 'helm-buffers-list)

;; Global keys definitions.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "s-p s a") 'helm-ag-project-root)

;; Set full ag search or only source.
(defun projectile-default-find-and-grep-commands ()
  (interactive)
  (setq helm-ag-base-command "ag --nocolor --nogroup -G\.(cpp|h|cc|cxx|el|xml|tex|txt|ini|pl|pm|html|js|sh)$|^.[^\.]+$")
  (setq projectile-generic-command "fdfind . -0 --type f --color=never -e cpp -e h -e cc -e el -e ini -e xml -e tex -e txt -e pl -e pm -e html -e js -e sh")
  (setq projectile-git-command "fdfind . -0 --type f --color=never -e cpp -e h -e cc -e el -e ini -e xml -e tex -e txt -e pl -e pm -e html -e js -e sh")
  (message "Setup ag and fdfind for: c++ lisp latex txt xml perl ini html js sh"))

(projectile-default-find-and-grep-commands) ;; Setup at startup.

(global-set-key (kbd "<s-f1>") 'projectile-default-find-and-grep-commands)
(global-set-key (kbd "<s-f2>") (lambda ()(interactive)
				 (setq helm-ag-base-command "ag --nocolor --nogroup -G\.(cxx|cc|cpp|h|hpp|c)$")
				 (setq projectile-generic-command "fdfind . -0 --type f --color=never -e cpp -e h -e cc -e el -e c -e hpp")
				 (setq projectile-git-command "fdfind . -0 --type f --color=never -e cpp -e h -e cc -e el -e c -e hpp")
				 (message "Setup ag and fdfind for: c++ only")))
(global-set-key (kbd "<s-f3>") (lambda ()(interactive)
				 (setq helm-ag-base-command "ag --nocolor --nogroup -G\.(xml)$")
				 (setq projectile-generic-command "fdfind . -0 --type f --color=never -e xml")
				 (setq projectile-git-command "fdfind . -0 --type f --color=never -e xml")
				 (message "Setup ag and fdfind for: xml only")))
(global-set-key (kbd "<s-f4>") (lambda ()(interactive)
				 (setq helm-ag-base-command "ag --nocolor --nogroup -G\.(tex)$")
				 (setq projectile-generic-command "fdfind . -0 --type f --color=never -e tex")
				 (setq projectile-git-command "fdfind . -0 --type f --color=never -e tex")
				 (message "Setup ag and fdfind for: LaTeX only")))

(global-set-key (kbd "<s-f5>") (lambda ()(interactive)
				 (setq helm-ag-base-command "ag --nocolor --nogroup -G\.(pl|pm)$")
				 (setq projectile-generic-command "fdfind . -0 --type f --color=never -e pl -e pm")
				 (setq projectile-git-command "fdfind . -0 --type f --color=never -e pl -e pm")
				 (message "Setup ag and fdfind for: Perl only")))

(global-set-key (kbd "<s-f6>") (lambda ()(interactive)
				 (setq helm-ag-base-command "ag --nocolor --nogroup -G\.(sh)$|^.[^\.]+$")
				 (setq projectile-generic-command "fdfind '^[^.]+$|^.*\.sh$' --color=never --type f")
				 (setq projectile-git-command "fdfind '^[^.]+$|^.*\.sh$' --color=never --type f")
				 (message "Setup ag and fdfind for: Shell Script or No extension")))

(global-set-key (kbd "<s-f7>") (lambda ()(interactive)
				 (setq helm-ag-base-command "ag --nocolor --nogroup -G\.(el)$|^.[^\.]+$")
				 (setq projectile-generic-command "fdfind . -0 --type f --color=never -e el")
				 (setq projectile-git-command "fdfind . -0 --type f --color=never -e el")
				 (message "Setup ag and fdfind for: Emacs LISP or No extension")))

;; Change keybinds to whatever you like :)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
(global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
(global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
(global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)

;; lsp.
(with-eval-after-load 'lsp-mode
  ;; (define-key lsp-mode-map (kbd "<f3>") 'helm-etags-select)
  (define-key lsp-mode-map (kbd "<f5>") 'compile-tools-make-linux64)
  (define-key lsp-mode-map (kbd "<f6>") 'compile-tools-run-perl-sics-install)
  (define-key lsp-mode-map (kbd "<f7>") 'lsp-ui-peek-find-references)
  (define-key lsp-mode-map (kbd "<f8>") 'helm-lsp-workspace-symbol)
  (define-key lsp-mode-map (kbd "<f9>") 'compile-tools-compile-make)
  (define-key lsp-mode-map (kbd "<f10>") 'gdb)
  (define-key lsp-mode-map (kbd "<f11>") 'lsp-ui-sideline-mode)
  (define-key lsp-mode-map (kbd "<f12>") 'edit-tools-insert-include)
)
;; rebihnd dired keys.
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<tab>") 'other-window)
  (define-key dired-mode-map (kbd "<f2>") 'find-name-dired)
  (define-key dired-mode-map (kbd "<f3>") 'find-grep-dired)
  (define-key dired-mode-map (kbd "<f4>") 'dired-do-find-regexp)
  (define-key dired-mode-map (kbd "<f5>") 'dired-do-copy)
  (define-key dired-mode-map (kbd "<f6>") 'dired-do-rename)
  (define-key dired-mode-map (kbd "<f7>") 'dired-do-create-files)
  (define-key dired-mode-map (kbd "<f8>") 'dired-do-delete)
  (define-key dired-mode-map (kbd "s") 'dired-sort)
  (define-key dired-mode-map (kbd ".") 'dired-omit-mode)
  ) 

;; (add-hook 'after-init-hook (lambda ()
;; 			     (define-key makefile-gmake-mode-map (kbd "<f5>") 'compile-tools-make-linux64)))

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "<f5>") 'magit-refresh)
  (define-key magit-mode-map (kbd "<f12>") 'magit-toggle-untracked-files))

(with-eval-after-load 'perl-mode
  ;; (define-key perl-mode-map (kbd "<f3>") 'helm-etags-select)
  (define-key perl-mode-map (kbd "<C-f3>") 'xref-pop-marker-stack)
  (define-key perl-mode-map (kbd "<f5>") 'execute-perl)
  (define-key perl-mode-map (kbd "<f6>") 'perldb)
  (define-key perl-mode-map (kbd "<f7>") 'compile-tools-run-perl-sics-install)
  (define-key perl-mode-map (kbd "<f8>") 'compile-tools-run-perl-create-package)
  (define-key perl-mode-map (kbd "<S-f8>") 'compile-tools-run-perl-create-package-force)
  (define-key perl-mode-map (kbd "<f11>") 'cperl-perldoc)
  (define-key perl-mode-map (kbd "<f12>") 'cperl-perldoc-at-point))

(provide 'keys-setup)
