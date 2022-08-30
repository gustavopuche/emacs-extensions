(require 'helm-config)
(require 'helm-grep)
(require 'helm-projectile)
(require 'helm-ag)

(use-package helm-gtags
  :delight)

;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "<f3>") 'helm-gtags-find-tag-from-here)
     (define-key helm-gtags-mode-map (kbd "<C-f3>") 'helm-gtags-pop-stack)))

(define-key global-map (kbd "M-,") 'xref-pop-marker-stack)

(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq
 helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
 helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
 helm-candidate-number-limit 500 ; limit the number of displayed canidates
 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
 helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non-nil
 helm-buffer-max-length nil
 )

;; (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)

;; (global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)

;; (global-set-key (kbd "C-c h x") 'helm-register)
;; ;; (global-set-key (kbd "C-x r j") 'jump-to-register)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)


(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)

;; ;; use helm to list eshell history
;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;; ;;; Save current position to mark ring
;; (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; ;; show minibuffer history with Helm
(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

;; (define-key global-map [remap find-tag] 'helm-etags-select)

(define-key global-map [remap list-buffers] 'helm-buffers-list)

;; Add as much extensions as you want to grep with helm-ag.
;; Initially only .cpp and .h files are searched.
(setq helm-ag-base-command "ag --nocolor --nogroup -G\.(cpp|h)$")
(global-set-key (kbd "s-p s a") 'helm-ag-project-root)

;; Set full ag search or only source.
(global-set-key (kbd "<s-f1>") (lambda () (interactive)
				 (setq helm-ag-base-command "ag --nocolor --nogroup -G\.(cpp|h|cxx|xml|tex|ini)$")
				 (setq projectile-generic-command "fdfind . -0 --type f --color=never -e cpp -e h-e cc -e el -e ini -e xml -e tex")
				 (setq projectile-git-command "fdfind . -0 --type f --color=never -e cpp -e h -e cc -e el -e ini -e xml -e tex")))
(global-set-key (kbd "<s-f2>") (lambda () (interactive)
				 (setq helm-ag-base-command "ag --nocolor --nogroup -G\.(cpp|h)$")
				 (setq projectile-generic-command "fdfind . -0 --type f --color=never -e cpp -e h-e cc -e el")
				 (setq projectile-git-command "fdfind . -0 --type f --color=never -e cpp -e h -e cc -e el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: helm-swoop                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'helm-swoop
  ;; Change keybinds to whatever you like :)
  (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)

  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)

  ;; Split direction.  'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)

  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color nil)

  ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t)

  ;; Optional face for line numbers
  ;; Face name is `helm-swoop-line-number-face`
  (setq helm-swoop-use-line-number-face t))

(require 'helm-org)

(provide 'helm-setup)
