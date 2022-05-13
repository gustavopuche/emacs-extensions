(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#100e23" "#ff8080" "#95ffa4" "#ffe9aa" "#91ddff" "#c991e1" "#aaffe4" "#BAC9E4"])
 '(calendar-week-start-day 1)
 '(custom-safe-themes
   '("b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "44ce31f709daeece2d4c19ce4f3899a5dabc7085543a5a2377bbeec8d5b38117" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "9bc1eec9b485726318efd9341df6da8b53fa684931d33beba57ed7207f2090d6" "234249a92c2cf7b61223d9f83e1d9eefcd80fcf6b7a5e9ca03dc9d3f1b122ae2" "b42cf9ee9e59c3aec585fff1ce35acf50259d8b59f3047e57df0fa38516aa335" "f07583bdbcca020adecb151868c33820dfe3ad5076ca96f6d51b1da3f0db7105" "dd3eb539595bd7643baaff3a3be67b735a82052c37c2d59192ef51a0983dbfca" "929744da373c859c0f07325bc9c8d5cc30d418468c2ecb3a4f6cb2e3728d4775" "c712d616ea5a9ef4e513681846eb908728bbb087c2d251ded8374ee9faafa199" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "2fc7672758572337a2c9d748d8f53cc7839244642e4409b375baef6152400b4d" "89127a6e23df1b1120aa61bd7984f1d5f2747cad1e700614a68bdb7df77189ba" "6ecfc451f545459728a4a8b1d44ac4cdcc5d93465536807d0cb0647ef2bb12c4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "5cd698ce53179b1e4eaa7917c992832a220600c973967904fea71e3980a46532" "beeb4fbb490f1a420ea5acc6f589b72c6f0c31dd55943859fc9b60b0c1091468" "35b4668e8858dba2d282534e5f221320caee7665ba8e434acc9d831481f21d4b" "83e7481811eba345af17cba126a0126eb999eb92c54df73d31622cd1ef14958b" default))
 '(european-calendar-style t)
 '(fci-rule-color "#858FA5")
 '(jdee-db-active-breakpoint-face-colors (cons "#100e23" "#906cff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100e23" "#95ffa4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100e23" "#565575"))
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(flycheck-irony ws-butler helm-gtags helm-rtags helm-org-rifle helm-eww helm-org-multi-wiki helm-org helm-ispell rainbow-delimiters highlight-numbers cmake-ide cmake-mode auto-yasnippet auto-complete-clang-async ac-clang company-rtags helm-company helm-ag helm-ack helm-flycheck helm-xref magit helm-lsp helm-projectile helm-swoop helm lsp-mode doom-themes yasnippet projectile hydra flycheck company avy which-key dap-mode))
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(vc-annotate-background "#1b182c")
 '(vc-annotate-color-map
   (list
    (cons 20 "#95ffa4")
    (cons 40 "#b8f7a6")
    (cons 60 "#dbf0a8")
    (cons 80 "#ffe9aa")
    (cons 100 "#ffd799")
    (cons 120 "#ffc488")
    (cons 140 "#ffb378")
    (cons 160 "#eda79b")
    (cons 180 "#db9cbd")
    (cons 200 "#c991e1")
    (cons 220 "#db8bc0")
    (cons 240 "#ed85a0")
    (cons 260 "#ff8080")
    (cons 280 "#d4757d")
    (cons 300 "#aa6a7a")
    (cons 320 "#805f77")
    (cons 340 "#858FA5")
    (cons 360 "#858FA5")))
 '(vc-annotate-very-old-color nil))


(set-frame-font "-outline-MSX Screen 0-normal-normal-normal-mono-14-*-*-*-c-*-iso10646-1")

(package-initialize)

;; Modules
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'themes-setup)
(require 'edit-tools)
(require 'org-setup)
(require 'irony-setup)
(require 'company-setup)
(require 'helm-setup)
(require 'lsp-setup)
(require 'projectile-setup)
(require 'licondefusr)
(require 'mailing)
(require 'text-order)
(require 'doxy-graph-mode)
(require 'compile-tools)
(require 'window-tools)

(require 'server)
(or (server-running-p)
    (server-start))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
