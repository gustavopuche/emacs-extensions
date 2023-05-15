(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#100e23" "#ff8080" "#95ffa4" "#ffe9aa" "#91ddff" "#c991e1" "#aaffe4" "#BAC9E4"])
 '(calendar-week-start-day 1)
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
 '(european-calendar-style t)
 '(exwm-floating-border-color "#c8c8c8")
 '(fci-rule-color "#858FA5")
 '(highlight-tail-colors
   ((("#e9f1e8" "#50a14f" "green")
     . 0)
    (("#e1eef3" "#0184bc" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#100e23" "#906cff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100e23" "#95ffa4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100e23" "#565575"))
 '(menu-bar-mode nil)
 '(objed-cursor-color "#e45649")
 '(org-agenda-files
   '("~/org/brain/STC-69647.org" "~/org/brain/STC-55249.org" "~/org/brain/STC-63293.org" "~/org/brain/STC-60883.org" "~/org/brain/STC-63814.org" "~/org/brain/STC-59592.org" "~/org/brain/STC-60603.org" "~/org/brain/STC-62887.org" "~/org/brain/STC-64341.org" "~/org/brain/STC-58735.org" "~/org/brain/STC-43305.org" "~/org/brain/STC-60075:Clear memory when handling user names and passwords.org" "~/org/brain/STC-62886:Enabler: Create motor scheduler feature - Auxiliary scheduler Control-Status signals.org" "~/org/brain/STC-61283:Check for new FW versions.org" "~/org/brain/STC-60118:Handle misallocated memory in CV403DriverHWInterface.org" "~/org/brain/STC-60341.org" "~/org/brain/STC-60778.org" "~/org/brain/STC-59372.org" "~/org/brain/STC-55176.org" "~/org/brain/STC-45190.org" "~/org/brain/STC-56843.org" "~/org/brain/STC-55669.org" "~/org/brain/STC-9026.org" "~/org/brain/STC-45537.org" "~/org/brain/organizer.org"))
 '(org-latex-to-mathml-convert-command "java -jar %j -unicode -force -df %o %I")
 '(org-latex-to-mathml-jar-file "/home/kjambunathan/Downloads/mathtoweb.jar")
 '(org-odt-convert-process "LibreOffice")
 '(org-odt-preferred-output-format "docx")
 '(org-odt-transform-processes
   '(("Optimize Column Width of all Tables" "soffice" "--norestore" "--invisible" "--headless" "macro:///OrgMode.Utilities.OptimizeColumnWidth(%I)")
     ("Update All" "soffice" "--norestore" "--invisible" "--headless" "macro:///OrgMode.Utilities.UpdateAll(%I)")
     ("Reload" "soffice" "--norestore" "--invisible" "--headless" "macro:///OrgMode.Utilities.Reload(%I)")))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("ox-odt" . "https://kjambunathan.github.io/elpa/")))
 '(package-selected-packages
   '(ox-odt all-the-icons-dired treemacs-all-the-icons calfw-howm calfw-cal calfw-ical calfw-org calfw graphviz-dot-mode vc-msg helm-swoop powerline lsp-ui delight drag-stuff srefactor cpp-auto-include noxml-fold web-mode-edit-element web-mode helm-gtags all-the-icons popup company-irony-c-headers company-irony flycheck-irony ws-butler helm-rtags helm-org-rifle helm-eww helm-org-multi-wiki helm-org helm-ispell rainbow-delimiters highlight-numbers cmake-ide cmake-mode auto-yasnippet auto-complete-clang-async ac-clang company-rtags helm-company helm-flycheck helm-xref magit lsp-mode doom-themes yasnippet projectile hydra flycheck company avy which-key dap-mode))
 '(pdf-view-midnight-colors (cons "#383a42" "#fafafa"))
 '(rustic-ansi-faces
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(tool-bar-mode nil)
 '(tramp-default-proxies-alist nil)
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
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-log-types '((org))))

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
