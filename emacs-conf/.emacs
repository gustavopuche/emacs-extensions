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
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile tramp-connection-local-default-profile)))
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
   '("f0efc56fa8db9560a39343a19e58e78ece8757ac1f7fd2f13b6212f1c34e8712" "eebb1b380ba81a0f61ffef333524a5fbab10e0c05d1a2864b67a2513b3007811" "6751b1b3a4fd0594bd530d1c8e1384f45595c7bf0ae44846bdc2dcf031dbd10b" "e0706e3735403d2a65001d40d02c1881bf465d82312a1800ad824e1340ff49bf" "28a1db17a20984b5e72aee1ec6f9aa4cd0179ee2c9a03e856ad036323052e6f2" "833ddec35949883eb8211c3300e54020a1dbd9aa1d55eb4d3deda19b99a69e11" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "052f06669458f446edbfbcc41fb117fd828944534cb7242630ec3bb270c9b8be" "2275bd7e2f4ad201ccfab56bdfb712f23336e94c0d792a83c3efb8643e58b891" "05dec9a94f86a1f2decb63ac647b497fbc69e54392b595c792ca77810173136c" "ee9b4799d3a6855541e96b0eb8e0f745d9e80d0dd1f1c63eaf54f9ac2b8e9356" "3c9f12072935a049e2d09bd446ca6e368bee57c120fc31b349adc2bf20bb3c57" "653f0dc25bcefb2448fbd66f95dfd6b2cc8726bf7c69ba43cc6ec653d59890b2" "45799ae89d708c77b5bb4a4c1d1a6fef5f7f106a6d47f69117f40e707d75c17e" "d64b57b5b16e403bce89cff69fb456a9ddce4f3697fcda09382c444f4c0cae3f" "562961c1c8900753a474545ce09fc1b3d28167f59661af93bcaca14b52ff0de9" "886490792b596142a9d31181efcad7e84b036fc01bca5fca1cb00acf3bf560e6" "6e621644dcb952aaaad6697feeb62ca8a2d4c2416df63124a3cf5228c3a153f2" "3cf02a78238619cf2d985d1b960abbaf8208c40d8baf4f61358bd674727b155c" "331755502d921429b8061e523a108c61f36f798f219eb60dd3b1212f46de60be" "56994f0226790d0da06eca14f78ba28263845cdea9f0175621a4f35b9622354d" "3c72a974e83fe2699c7732aa90315ca6043db8dd70e543cdb8b49895c1662444" "f849c012648b9533bb77fe3466c8c71a78d4848a8b7b62bcd5ce1cb834e2f93c" "9c2a6d68f61b3a335c24dfec7de2f606778285c5f0cfe15d390ce48ec8f8292e" "2997d33967afab511c3b59b992900ff4b7aa58f45ff8fa516326a028d78a4da0" "4df2281827cb92d08e41ff84d1f06eaa41d9091f65118c0103260c6da25f6415" "d8f29bb9a54609552fd6852eca32454edbfa0adb7230d9cb9f5f2685cd04c90d" "3d99c219d8a2ad6265f8111d40ddf2bd75d3dcc7950b22204a461f229a44be0d" "58f8c9802e4f14c74115d2c197e1f596dc6a28dab6ed1faa7c6c1a41628ad9fe" "6402dd4b7b555968332f15bc0bb62044887093d1980d5c9a79029c55ed1c7ba2" "851e3d0be36574c6fb0fea16a9aeea3daa8070979e770ff30b105d34d9c23304" "9a176ee931f5f7ba034669739edb1833f32a0ef4fcf859a3f688bca218a740eb" "2ef3c4c43cba85ee0d597efddcbb8e07ac288f7d0f9c0cb928dc8491cacb0482" "103b4d02fc98c0a71f491ae408f6e8f45abcfd866f5fbbd9e74e70bae6cfb072" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "945b6e04818e0f44af19bf60b993b2285aefa801fa096f11a03bc47f707a26af" "af5f323a26ae0e43281bf227e102bde79ed967e9fa0ec5f1f87d739b641a35c3" "55bb2c9bed1ee5a7ea605c5879548642ce5a3c449bdf2af140677debf22832eb" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "380c35199b5b32fe11c4b1f094b69c00d849d062206873a58e9a77ea7cea95e6" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "068145397c9c8679fa7d4bc0138f8bbd757d9eaf1afb2f77bdf161ade9a5c79f" "dd12fc8c4ef9c184e863fbe502e9147fd8772cd3fd276a513d71f57445b4eea3" "439ef224425e9dcb2b31b2228f85f11da26bbc09547a4b27da2fcba74c5fb8ba" "7f8fa1f7ab843a73045b5ec5d4afea51e43da8eaea3ef14caa175f83da9ff70d" "353ecb2763507b28db5677fa23d361567e27386cf96d22d74d2c310814bd6dfb" "a5cc8eb897062f8b47f33b29161b2400586bd313e73af1546efc8712fff90297" "db481e029169d07475de4c20a5a922a6d99185a9d135a59bfd0199c114431662" "b79ca369a8ba2f361858c16c23e1e469a30a258a1fa8e4842a82bf392c17e7a3" "151d4ec73a96bc0e9be5b300393710e85d1fc71366af8d18220bebd0d7ef4661" "d1334a593e7e5c4ff1b1e63bee20bf7ad0e75dfce5dc0c7a49549b903f7076cf" "6782a1168094e7ef98f76a567bff47de8c896f50b831a0abc3eea1b4f946e91a" "87337c8bb6d0e10e5818daacf9d1bedbb222ec7bed865d2de5e3150b768b805e" "8c578025de6c75bce8fe4eee29d8caf13cf50b23745f01410cc755ae25657638" "03d0d6d3adc10b477c85bea43f7e9bcfec67152519e0b51596acf97430b3f066" "7a663243824c061e12cb009b0000ccf88632d237afffc0040402ddcf3f9c972a" "286fb345a3d7b667c6f89a26e2ba92c1cb5a6896c158ad5459792d3c10489c27" "0d180b84dddecda30c00b383be7f64a18c0ad14c177105a716273e3ce3b73a47" "42db06c2faeb1b0e88ab7ea56c1cb26da507bb2d88ce7032ddb8e6e0b10c7739" "f34008749941a3eae23c302658052133171253d603c4ea1903f2ce564bc50ba9" "5fa02336dc394ed10a6cae6852b5d8d7ace9d8c0c7a8dafa51ed514be13ce8b4" "e9ccc031436358a5378361c742cc2cb0cd9ea356ee93fb78dde6febf3675d164" "ad0eacd8aad1dc2485c12954e928863201ad3c800fc8d8cfc9e9226698cbe337" "833871173a9d257ae8cc46738adb9cbe0ee044de16218e177a27d15a4896407c" "e87197ac5a0205702c0d709faf55f8e6653984fb959743a184bf9ac44bec7898" "b5d339ae8fe5cdf4888c7c7bc6ccea83c93b6a93186a60a9447555bd16e649f0" "1b62514c663914e3b3d750695f4504aa7cbb507f5de66ec34eaee78a43ac24e3" "31f31c0aba4b496aa4013ab45ce5e93ac7c6fda025706a0ab8d6162a6dc4c5fc" "ec6d2c3c2295fb4dab82da1a5b6ac992ad99631d2dbf2f2b7173e2f7e5ceaa7a" "7530e7d2ec4f66bac74fe1409f860dce70c3e3fa5f2f801cd248138c8effe18f" "a27b9e11c25dc455f3d54280230c2cd2eb96e1fc3843bc11e5924ed9ea527e7e" "e0c8b8175dff079c55eafadee381922c1370178b874f55beed2399bc5f49e556" default))
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
   '(dired-iconq org-kanban dired-icon dired-du disaster org-roam graphviz-dot-mode lsp-ui flycheck-irony helm irony cmake-mode magit yasnippet ws-butler which-key use-package treemacs-all-the-icons srefactor rainbow-delimiters powerline ox-odt helm-swoop helm-projectile helm-org helm-lsp helm-gtags helm-grepint helm-ag gnu-elpa-keyring-update flycheck drag-stuff doom-themes delight dap-mode company-irony-c-headers company-irony))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tramp-default-proxies-alist nil)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((emacs) (use-package) (emacs))))

(package-initialize)

;; Modules
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("ox-odt" . "https://kjambunathan.github.io/elpa/") t)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") 
(package-refresh-contents)
(message "packages are refreshed")
(unless (package-installed-p 'use-package)
       (package-install 'use-package))
;; (use-package gnu-elpa-keyring-update :ensure t)
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
 '(flycheck-error ((t (:underline (:color "#fc618d" :style wave :position wave)))))
 '(org-default ((t (:inherit default))))
 '(org-extra-emphasis ((t (:height 2.0 :inherit default :weight black :width normal))) t)
 '(org-extra-emphasis-01 ((t (:family "EnhancedDotDigital-7" :foreground "#A53E2D" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-02 ((t (:family "EnhancedDotDigital-7" :foreground "ForestGreen" :inherit org-extra-emphasis :weight bold))))
 '(org-extra-emphasis-03 ((t (:family "EnhancedDotDigital-7" :foreground "gold2" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-04 ((t (:family "EnhancedDotDigital-7" :foreground "cyan3" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-05 ((t (:family "EnhancedDotDigital-7" :foreground "#54436E" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-06 ((t (:family "EnhancedDotDigital-7" :foreground "#D65D8F" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-07 ((t (:family "EnhancedDotDigital-7" :foreground "#E5C494" :inherit org-extra-emphasis :weight extra-bold))))
 '(org-extra-emphasis-08 ((t (:family "EnhancedDotDigital-7" :foreground "DarkOrange1" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-09 ((t (:family "EnhancedDotDigital-7" :foreground "firebrick1" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-10 ((t (:family "EnhancedDotDigital-7" :foreground "SpringGreen3" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-11 ((t (:family "EnhancedDotDigital-7" :foreground "yellow3" :inherit org-extra-emphasis :weight bold))))
 '(org-extra-emphasis-12 ((t (:family "EnhancedDotDigital-7" :foreground "DeepSkyBlue2" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-13 ((t (:family "EnhancedDotDigital-7" :foreground "magenta4" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-14 ((t (:family "EnhancedDotDigital-7" :foreground "burlywood4" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-15 ((t (:family "EnhancedDotDigital-7" :foreground "sienna2" :inherit org-extra-emphasis))))
 '(org-extra-emphasis-16 ((t (:family "EnhancedDotDigital-7" :foreground "SlateBlue3" :inherit org-extra-emphasis :weight extra-bold)))))
