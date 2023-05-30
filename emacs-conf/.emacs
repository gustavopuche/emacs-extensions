(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(column-number-mode t)
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("6782a1168094e7ef98f76a567bff47de8c896f50b831a0abc3eea1b4f946e91a" "87337c8bb6d0e10e5818daacf9d1bedbb222ec7bed865d2de5e3150b768b805e" "8c578025de6c75bce8fe4eee29d8caf13cf50b23745f01410cc755ae25657638" "03d0d6d3adc10b477c85bea43f7e9bcfec67152519e0b51596acf97430b3f066" "7a663243824c061e12cb009b0000ccf88632d237afffc0040402ddcf3f9c972a" "286fb345a3d7b667c6f89a26e2ba92c1cb5a6896c158ad5459792d3c10489c27" "0d180b84dddecda30c00b383be7f64a18c0ad14c177105a716273e3ce3b73a47" "42db06c2faeb1b0e88ab7ea56c1cb26da507bb2d88ce7032ddb8e6e0b10c7739" "f34008749941a3eae23c302658052133171253d603c4ea1903f2ce564bc50ba9" "5fa02336dc394ed10a6cae6852b5d8d7ace9d8c0c7a8dafa51ed514be13ce8b4" "e9ccc031436358a5378361c742cc2cb0cd9ea356ee93fb78dde6febf3675d164" "ad0eacd8aad1dc2485c12954e928863201ad3c800fc8d8cfc9e9226698cbe337" "833871173a9d257ae8cc46738adb9cbe0ee044de16218e177a27d15a4896407c" "e87197ac5a0205702c0d709faf55f8e6653984fb959743a184bf9ac44bec7898" "b5d339ae8fe5cdf4888c7c7bc6ccea83c93b6a93186a60a9447555bd16e649f0" "1b62514c663914e3b3d750695f4504aa7cbb507f5de66ec34eaee78a43ac24e3" "31f31c0aba4b496aa4013ab45ce5e93ac7c6fda025706a0ab8d6162a6dc4c5fc" "ec6d2c3c2295fb4dab82da1a5b6ac992ad99631d2dbf2f2b7173e2f7e5ceaa7a" "7530e7d2ec4f66bac74fe1409f860dce70c3e3fa5f2f801cd248138c8effe18f" "a27b9e11c25dc455f3d54280230c2cd2eb96e1fc3843bc11e5924ed9ea527e7e" "e0c8b8175dff079c55eafadee381922c1370178b874f55beed2399bc5f49e556" default))
 '(font-use-system-font t)
 '(ispell-dictionary nil)
 '(menu-bar-mode nil)
 '(org-agenda-files '("~/org/brain/STC-55249.org"))
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
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((comp))))

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
(require 'dired-setup)
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

(set-face-attribute 'default nil :font "CPMono_v07 Bold" :height 98)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
