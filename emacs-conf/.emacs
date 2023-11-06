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
   '("afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "945b6e04818e0f44af19bf60b993b2285aefa801fa096f11a03bc47f707a26af" "af5f323a26ae0e43281bf227e102bde79ed967e9fa0ec5f1f87d739b641a35c3" "55bb2c9bed1ee5a7ea605c5879548642ce5a3c449bdf2af140677debf22832eb" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "380c35199b5b32fe11c4b1f094b69c00d849d062206873a58e9a77ea7cea95e6" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "068145397c9c8679fa7d4bc0138f8bbd757d9eaf1afb2f77bdf161ade9a5c79f" "dd12fc8c4ef9c184e863fbe502e9147fd8772cd3fd276a513d71f57445b4eea3" "439ef224425e9dcb2b31b2228f85f11da26bbc09547a4b27da2fcba74c5fb8ba" "7f8fa1f7ab843a73045b5ec5d4afea51e43da8eaea3ef14caa175f83da9ff70d" "353ecb2763507b28db5677fa23d361567e27386cf96d22d74d2c310814bd6dfb" "a5cc8eb897062f8b47f33b29161b2400586bd313e73af1546efc8712fff90297" "db481e029169d07475de4c20a5a922a6d99185a9d135a59bfd0199c114431662" "b79ca369a8ba2f361858c16c23e1e469a30a258a1fa8e4842a82bf392c17e7a3" "151d4ec73a96bc0e9be5b300393710e85d1fc71366af8d18220bebd0d7ef4661" "d1334a593e7e5c4ff1b1e63bee20bf7ad0e75dfce5dc0c7a49549b903f7076cf" "6782a1168094e7ef98f76a567bff47de8c896f50b831a0abc3eea1b4f946e91a" "87337c8bb6d0e10e5818daacf9d1bedbb222ec7bed865d2de5e3150b768b805e" "8c578025de6c75bce8fe4eee29d8caf13cf50b23745f01410cc755ae25657638" "03d0d6d3adc10b477c85bea43f7e9bcfec67152519e0b51596acf97430b3f066" "7a663243824c061e12cb009b0000ccf88632d237afffc0040402ddcf3f9c972a" "286fb345a3d7b667c6f89a26e2ba92c1cb5a6896c158ad5459792d3c10489c27" "0d180b84dddecda30c00b383be7f64a18c0ad14c177105a716273e3ce3b73a47" "42db06c2faeb1b0e88ab7ea56c1cb26da507bb2d88ce7032ddb8e6e0b10c7739" "f34008749941a3eae23c302658052133171253d603c4ea1903f2ce564bc50ba9" "5fa02336dc394ed10a6cae6852b5d8d7ace9d8c0c7a8dafa51ed514be13ce8b4" "e9ccc031436358a5378361c742cc2cb0cd9ea356ee93fb78dde6febf3675d164" "ad0eacd8aad1dc2485c12954e928863201ad3c800fc8d8cfc9e9226698cbe337" "833871173a9d257ae8cc46738adb9cbe0ee044de16218e177a27d15a4896407c" "e87197ac5a0205702c0d709faf55f8e6653984fb959743a184bf9ac44bec7898" "b5d339ae8fe5cdf4888c7c7bc6ccea83c93b6a93186a60a9447555bd16e649f0" "1b62514c663914e3b3d750695f4504aa7cbb507f5de66ec34eaee78a43ac24e3" "31f31c0aba4b496aa4013ab45ce5e93ac7c6fda025706a0ab8d6162a6dc4c5fc" "ec6d2c3c2295fb4dab82da1a5b6ac992ad99631d2dbf2f2b7173e2f7e5ceaa7a" "7530e7d2ec4f66bac74fe1409f860dce70c3e3fa5f2f801cd248138c8effe18f" "a27b9e11c25dc455f3d54280230c2cd2eb96e1fc3843bc11e5924ed9ea527e7e" "e0c8b8175dff079c55eafadee381922c1370178b874f55beed2399bc5f49e556" default))
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
   '(org-roam graphviz-dot-mode lsp-ui flycheck-irony helm irony cmake-mode magit yasnippet ws-butler which-key use-package treemacs-all-the-icons srefactor rainbow-delimiters powerline ox-odt helm-swoop helm-projectile helm-org helm-lsp helm-gtags helm-grepint helm-ag gnu-elpa-keyring-update flycheck drag-stuff doom-themes delight dap-mode company-irony-c-headers company-irony))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((emacs) (use-package) (emacs))))

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
 '(flycheck-error ((t (:underline (:color "#fc618d" :style wave :position wave))))))
