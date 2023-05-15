(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-safe-themes
   '("7530e7d2ec4f66bac74fe1409f860dce70c3e3fa5f2f801cd248138c8effe18f" "a27b9e11c25dc455f3d54280230c2cd2eb96e1fc3843bc11e5924ed9ea527e7e" "e0c8b8175dff079c55eafadee381922c1370178b874f55beed2399bc5f49e556" default))
 '(font-use-system-font t)
 '(ispell-dictionary nil)
 '(menu-bar-mode nil)
 '(org-latex-to-mathml-convert-command "java -jar %j -unicode -force -df %o %I")
 '(org-latex-to-mathml-jar-file "/home/kjambunathan/Downloads/mathtoweb.jar")
 '(org-odt-convert-process "LibreOffice")
 '(org-odt-preferred-output-format "docx")
 '(org-odt-transform-processes
   '(("Optimize Column Width of all Tables" "soffice" "--norestore" "--invisible" "--headless" "macro:///OrgMode.Utilities.OptimizeColumnWidth(%I)")
     ("Update All" "soffice" "--norestore" "--invisible" "--headless" "macro:///OrgMode.Utilities.UpdateAll(%I)")
     ("Reload" "soffice" "--norestore" "--invisible" "--headless" "macro:///OrgMode.Utilities.Reload(%I)")))
 '(package-selected-packages
   '(cmake-mode magit yasnippet ws-butler which-key use-package treemacs-all-the-icons srefactor rainbow-delimiters powerline ox-odt helm-swoop helm-projectile helm-org helm-lsp helm-gtags helm-grepint helm-ag gnu-elpa-keyring-update flycheck drag-stuff doom-themes delight dap-mode company-irony-c-headers company-irony))
 '(tool-bar-mode nil))

(package-initialize)

;; Modules
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("ox-odt" . "https://kjambunathan.github.io/elpa/") t)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") 
(package-refresh-contents)
(message "packages are refreshed")
(unless (package-installed-p 'use-package)
       (package-install 'use-package))
(use-package gnu-elpa-keyring-update :ensure t)
(use-package delight :ensure t)
(setq load-prefer-newer t)

(require 'themes-setup)
(require 'edit-tools)
(require 'org-setup)
(require 'irony-setup)
(require 'company-setup)
(require 'helm-setup)
(require 'lsp-setup)
(require 'projectile-setup)
;; (require 'licondefusr)
;; (require 'mailing)
(require 'text-order)
(require 'doxy-graph-mode)
(require 'compile-tools)
(require 'window-tools)
(require 'keys-setup)

(require 'server)
(or (server-running-p)
    (server-start))

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "CPMono_v07" :foundry "PYRS" :slant normal :weight light :height 98 :width normal)))))
