;;; compile-tools.el --- Compile settings

;; Copiright (C) 2020 Gustavo Puche

;; Author: Gustavo Puche <gustavo.puche@gmail.com>
;; Created: 30 July 2020
;; Version: 0.2
;; Keywords: languages all
;; Package-Requires:


;;; Commentary:
;; Settup keybindings for compile commands.

;;; Code:

(require 'rainbow-delimiters)

;; Environment variables
(defvar compile-tools--target nil
  "Qt TARGET.")

(defvar compile-tools--qtdir nil
  "QTDIR path.")

(defvar compile-tools--qtdebug nil
  "Debug mode flag.")

(defvar compile-tools--qtdir-linux "/opt/extra/qt/5.12.3/gcc_64"
  "QTDIR path.")

(defvar compile-tools--qtdir-android-v8a "/opt/extra/qt/5.12.3/android_arm64_v8a"
  "QTDIR path.")

(defvar compile-tools--qtdir-android-v7 "/opt/extra/qt/5.12.3/android_armv7"
  "QTDIR path.")

(defvar compile-tools--qtdir-android-5-15-8 "/opt/extra/qt/5.15.8/android"
  "QTDIR path.")

(defvar compile-tools--clang-flags nil
  "clang debug flags")

(defvar compile-tools--qmake-flags nil
  "qmake android target flags")

(defvar compile-tools--clang-flags-linux "-spec linux-clang CONFIG+=debug CONFIG+=qml_debug"
  "clang debug flags")

(defvar compile-tools--clang-flags-android "-spec android-clang CONFIG+=debug CONFIG+=qml_debug"
  "clang debug flags")

(defvar compile-tools--clang-flags-android-release "-spec android-clang CONFIG+=qtquickcompiler"
  "clang debug flags")

(defvar compile-tools--android-ndk-root "/opt/extra/android/android-ndk-r19c"
  "Android NDK path.")

(defvar compile-tools--qt-build-path nil
  "Qt Build path.")

(defvar compile-tools--qt-pro-file "~/share/workspace/logger-qt/logger-suite.pro"
  "Qt Build path.")

;; Install apk into device.
;; "/opt/extra/qt/5.15.8/android/bin/androiddeployqt --verbose --output /opt/extra/workbench/build-tweety-Android_Qt_5_15_3_Clang_Multi_Abi-Debug/android-build --no-build --input /opt/extra/workbench/build-tweety-Android_Qt_5_15_3_Clang_Multi_Abi-Debug/android-displaynoteapp-deployment-settings.json --gradle --install --device R52N706N32D"

;;/opt/extra/android/platform-tools/adb -s R52N706N32D  install -r /opt/extra/workbench/build-tweety-Android_Qt_5_12_3_Clang_arm64_v8a-Debug/android-build//build/outputs/apk/debug/android-build-debug.apk

;; TODO setup android deploy
;; "/opt/extra/qt/5.12.3/android_armv7/bin/androiddeployqt --verbose --output /opt/extra/workbench/googlecast-qt/build-googlecasttest-Android_Qt_5_12_3_Clang_armeabi_v7a-Debug/android-build --no-build --input /opt/extra/workbench/googlecast-qt/build-googlecasttest-Android_Qt_5_12_3_Clang_armeabi_v7a-Debug/android-libgooglecasttest.so-deployment-settings.json --gradle --reinstall --device R52N706N32D"

;; example of setting env var named “path”, by appending a new path to existing path
(setenv "ANDROID_HOME" "/opt/extra/android")
(setenv "ANDROID_NDK_HOST" "linux-x86_64")
(setenv "ANDROID_NDK_PLATFORM" "android-21")
(setenv "ANDROID_NDK_ROOT" "/opt/extra/android/ndk/21.3.6528147")
(setenv "ANDROID_SDK_ROOT" "/opt/extra/android")

;; Path for depod
(setenv "PATH" (concat (getenv "PATH") ":/opt/extra/workbench/depot_tools"))

;; First Android device from adb list.
(defun compile-tools-android-device ()
  "Gets first device in adb devices list"
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "adb devices|sed -n '2p'|cut -f1")))

;; Set NDK for Qt version.
(defun compile-tools-set-ndk-qt (qt-version)
  "Sets NDK according to Qt version.
Qt 5.12 uses ndk/19.2.5345600
Qt 5.15 uses ndk/21.3.6528147"
  (setenv "ANDROID_NDK_PLATFORM" "android-21")
  (if (equal qt-version "5.12")
      (setenv "ANDROID_NDK_ROOT" "/opt/extra/android/ndk/19.2.5345600")
    (setenv "ANDROID_NDK_ROOT" "/opt/extra/android/ndk/21.3.6528147")))

;; Important to reuse
(defun compile-tools-set-target ()
  "Minibuffer chooser of TARGET options"
  (interactive)
  (setq compile-tool--target (completing-read "Choose TARGET: " '("linux" "Android ARMv8a" "Android ARMv7" "Android Qt5.15.8 Debug" "Android Qt5.15.8 Release" "Android Qt5.15.8 ARMv7 only" "Android Qt5.15.8 ARMv8a only") nil t))
  (message "You chose `%s'" compile-tool--target)
  (if (equal compile-tool--target "linux")
      ;; Linux
      (progn
	(setq compile-tools--qtdir compile-tools--qtdir-linux)
	(setq compile-tools--clang-flags compile-tools--clang-flags-linux))
    (if (equal compile-tool--target "Android Qt5.15.8 Debug")
	;; Qt 5.15.8 for Android
	(progn
	  (message "This is the way")
	  (setq compile-tools--qtdir compile-tools--qtdir-android-5-15-8)
	  (setq compile-tools--clang-flags compile-tools--clang-flags-android)
	  (setq compile-tools--qmake-flags "ANDROID_ABIS=\"armeabi-v7a arm64-v8a\"")
	  (compile-tools-set-ndk-qt "5.15"))
      (if (equal compile-tool--target "Android Qt5.15.8 Release")
	  ;; Qt 5.15.8 for Android
	  (progn
	    (message "This is the way")
	    (setq compile-tools--qtdir compile-tools--qtdir-android-5-15-8)
	    (setq compile-tools--clang-flags compile-tools--clang-flags-android-release)
	    (setq compile-tools--qmake-flags "ANDROID_ABIS=\"armeabi-v7a arm64-v8a\"")
            (compile-tools-set-ndk-qt "5.15"))
        (if (equal compile-tool--target "Android Qt5.15.8 ARMv7 only")
            (progn
              (message "This is the way")
	      (setq compile-tools--qtdir compile-tools--qtdir-android-5-15-8)
	      (setq compile-tools--clang-flags compile-tools--clang-flags-android)
	      (setq compile-tools--qmake-flags "ANDROID_ABIS=\"armeabi-v7a\"")
              (compile-tools-set-ndk-qt "5.15"))
          (if (equal compile-tool--target "Android Qt5.15.8 ARMv8a only")
              (progn 
                (message "This is the way")
		(setq compile-tools--qtdir compile-tools--qtdir-android-5-15-8)
		(setq compile-tools--clang-flags compile-tools--clang-flags-android)
		(setq compile-tools--qmake-flags "ANDROID_ABIS=\"arm64-v8a\"")
                (compile-tools-set-ndk-qt "5.15"))
	    (if (equal compile-tool--target "Android ARMv8a")
		;; Android v8
		(progn
		  (setq compile-tools--qtdir compile-tools--qtdir-android-v8a)
		  (setq compile-tools--clang-flags compile-tools--clang-flags-android)
		  (compile-tools-set-ndk-qt "5.12"))
	      ;; Android v7
	      (progn
		(setq compile-tools--qtdir compile-tools--qtdir-android-v7)
		(setq compile-tools--clang-flags compile-tools--clang-flags-android)
                (compile-tools-set-ndk-qt "5.12")
		(setenv "ANDROID_NDK_PLATFORM" "android-16")))))))))

(defun compile-tools-qt-debug-mode-p ()
  "Returns t if qt debug
else returns nil"
  (if (null compile-tools--clang-flags)
      t
    (if (member "CONFIG+=debug" (split-string compile-tools--clang-flags))
        t
      nil)))

(defun compile-tools-qt-debug-mode ()
  (if (compile-tools-qt-debug-mode-p)
      "debug"
    "release"))

(defun compile-tools-get-target-qtdir ()
  ""
  (interactive)
  (if (null compile-tools--qtdir)
      (compile-tools-set-target))
  (message compile-tools--qtdir))

(defun compile-tools-reset-target ()
  "Reset android qt variables."
  (interactive)
  (setq compile-tools--qtdir nil)
  (setq compile-tools--clang-flags nil)
  (setq compile-tools--qt-build-path nil)
  (message "Reset android qt stuff."))

(defun compile-tools-pro-file ()
  "Gets Qt main PRO file."
  ;; (if (file-exists-p (concat (projectile-project-root) (projectile-project-name) ".pro"))
  ;;     (concat (projectile-project-root) (projectile-project-name) ".pro")
  ;;   (car (file-expand-wildcards (concat (projectile-project-root) "*.pro"))))
  buffer-file-name)

(defun compile-tools-get-deploy-file ()
  "Gets Qt Android deploy file."
  (car (file-expand-wildcards (concat compile-tools--qt-build-path "/*.json"))))

(defun compile-tools-gtest-file ()
  "Gets Google Test EXE file."
  (concat (projectile-project-root) "tests/GTestBackend/packages/Linux/gcc_64/Debug/bin/GTestBackend"))

;; Sets Qt project build path
(defun compile-tools-set-qt-build-path ()
  "Opens a directory chooser and setup `compile-tools--qt-build-path'."
  (interactive)
  (setq compile-tools--qt-build-path (read-directory-name "Please choose Qt project build folder:")))

(defun compile-tools-qmake ()
  "Execute qmake in build folder.
COMMANDS
/opt/extra/Qt-linux-5.12.3/5.12.3/gcc_64/bin/qmake ~/share/workspace/logger-qt/logger-suite.pro -spec linux-clang CONFIG+=debug CONFIG+=qml_debug

make -f ./Makefile qmake_all"
  (interactive)
  (setq compile-tools--qt-build-path
        (concat (projectile-project-root) "build/" (compile-tools-qt-debug-mode)))
  (message compile-tools--qt-build-path)
  (if (not (file-exists-p compile-tools--qt-build-path))
      (make-directory compile-tools--qt-build-path :parents))
  (compile (concat "cd " compile-tools--qt-build-path
		   " && "
		   (compile-tools-get-target-qtdir) "/bin/qmake" " " compile-tools--qmake-flags " " (compile-tools-pro-file) " " compile-tools--clang-flags
		   " && "
		   "make -f ./Makefile qmake_all"))
  )

(defun compile-tools-compile-make ()
  "Execute make command."
  (interactive)
  (if (null compile-tools--qt-build-path)
      (compile "make -j4")
    (compile (concat "cd " compile-tools--qt-build-path
		     " && "
		     "make -j4"))))

(defun compile-tools-compile-make-run ()
  "Execute make run command."
  (interactive)
  (compile "make run"))

(defun compile-tools-compile-make-clean ()
  "Execute make clean command."
  (interactive)
  (if (null compile-tools--qt-build-path)
      (compile "make clean")
    (compile (concat "cd " compile-tools--qt-build-path
		     " && "
		     "make clean"))))

(defun compile-tools-compile-make-apk ()
  "Execute make apk command."
  (interactive)
  (if (null compile-tools--qt-build-path)
      (compile "make -j4 apk")
    (compile (concat "cd " compile-tools--qt-build-path
		     " && "
		     "make -j4 apk"))))

(defun compile-tools-compile-make-aab ()
  "Execute make aab command."
  (interactive)
  (if (null compile-tools--qt-build-path)
      (compile "make -j4 apk")
    (compile (concat "cd " compile-tools--qt-build-path
		     " && "
		     "make -j4 aab"))))

(defun compile-tools-compile-make-test ()
  "Execute make test-pc command."
  (interactive)
  (compile "make test-pc"))

(defun compile-tools-run-gtest ()
  "Execute make run command."
  (interactive)
  (compile (compile-tools-gtest-file)))

(defun compile-tools-debug ()
  "Debug"
  (interactive)
  (message (car (file-expand-wildcards (concat (projectile-project-root) "*.pro")))))

(defun compile-tools-debug-gtest ()
  "Debug gtest-file"
  (interactive)
  (compile-tools-gtest-file))

(defun compile-tools-compile-cmake ()
  "Execute cmake command."
  (interactive)
  (compile "cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1"))

(defun compile-tools-compile-make-install ()
  "Execute make run command."
  (interactive)
  (if (null compile-tools--qt-build-path)
      (compile "make install")
    (compile (concat "cd " compile-tools--qt-build-path
		     " && "
		     "make install"))))

(defun compile-tools-compile-make-qt-android-install ()
  "Execute make run command."
  (interactive)
  (if (null compile-tools--qt-build-path)
      (compile "make install")
    (compile (concat "cd " compile-tools--qt-build-path
		     " && "
		     "make install INSTALL_ROOT=" compile-tools--qt-build-path "/android-build"))))

;; Android qt device deploy.
(defun compile-tools-qt-android-deploy ()
  "Deploy into Android apk or aab.
COMMANDS
/opt/extra/qt/5.12.3/android_arm64_v8a/bin/androiddeployqt --input /opt/extra/workbench/build-tweety-Android_Qt_5_12_3_Clang_arm64_v8a-Debug/android-libdisplaynoteapp.so-deployment-settings.json --output /opt/extra/workbench/build-tweety-Android_Qt_5_12_3_Clang_arm64_v8a-Debug/android-build --android-platform android-30 --jdk /usr/lib/jvm/java-8-openjdk-amd64 --gradle

/opt/extra/qt/5.12.3/android_armv7/bin/androiddeployqt --verbose --output /opt/extra/workbench/googlecast-qt/build-googlecasttest-Android_Qt_5_12_3_Clang_armeabi_v7a-Debug/android-build --no-build --input /opt/extra/workbench/googlecast-qt/build-googlecasttest-Android_Qt_5_12_3_Clang_armeabi_v7a-Debug/android-libgooglecasttest.so-deployment-settings.json --gradle --reinstall --device R52N706N32D"
  (interactive)
  (compile (concat compile-tools--qtdir "/bin/androiddeployqt --verbose --output "
                   compile-tools--qt-build-path "/android-build  --input "
                   (compile-tools-get-deploy-file) " --android-platform android-30 --jdk /usr/lib/jvm/java-8-openjdk-amd64 --gradle")))

;; Android qt device deploy device.
(defun compile-tools-qt-android-deploy-device ()
  "Deploy into Android device.
COMMANDS
/opt/extra/qt/5.12.3/android_arm64_v8a/bin/androiddeployqt --input /opt/extra/workbench/build-tweety-Android_Qt_5_12_3_Clang_arm64_v8a-Debug/android-libdisplaynoteapp.so-deployment-settings.json --output /opt/extra/workbench/build-tweety-Android_Qt_5_12_3_Clang_arm64_v8a-Debug/android-build --android-platform android-30 --jdk /usr/lib/jvm/java-8-openjdk-amd64 --gradle

/opt/extra/qt/5.12.3/android_armv7/bin/androiddeployqt --verbose --output /opt/extra/workbench/googlecast-qt/build-googlecasttest-Android_Qt_5_12_3_Clang_armeabi_v7a-Debug/android-build --no-build --input /opt/extra/workbench/googlecast-qt/build-googlecasttest-Android_Qt_5_12_3_Clang_armeabi_v7a-Debug/android-libgooglecasttest.so-deployment-settings.json --gradle --reinstall --device R52N706N32D"
  (interactive)
  (compile (concat compile-tools--qtdir "/bin/androiddeployqt --verbose --output "
                   compile-tools--qt-build-path "/android-build  --input "
                   (compile-tools-get-deploy-file) " --gradle --reinstall --device " (compile-tools-android-device))))

(defun compile-tools-qt-android-run ()
  "Execute apk App."
  "adb-run.sh myApp.apk")

(defun compile-tools-compile-make-qt-android-run ()
  "Deploy and run Android App with Qt."
  (interactive)
  (compile (concat  "cd " compile-tools--qt-build-path
                    " && "
		    "make -j4 install INSTALL_ROOT=" compile-tools--qt-build-path "/android-build"
                    " && "
                    compile-tools--qtdir "/bin/androiddeployqt --verbose --output "
                    compile-tools--qt-build-path "/android-build  --input "
                    (compile-tools-get-deploy-file) " --android-platform android-30 --jdk /usr/lib/jvm/java-8-openjdk-amd64 --gradle"
                    " && "
                    compile-tools--qtdir "/bin/androiddeployqt --verbose --output "
                    compile-tools--qt-build-path "/android-build  --input "
                    (compile-tools-get-deploy-file) " --gradle --reinstall --device " (compile-tools-android-device))))

;; C++ hooks.
(add-hook 'c-mode-hook 'doxy-graph-mode)
(add-hook 'c++-mode-hook 'doxy-graph-mode)
(add-hook 'python-mode-hook 'doxy-graph-mode)
(add-hook 'c++-mode-hook 'highlight-numbers-mode)
(add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c-mode-common-hook 'ws-butler-mode)

;; Lisp hooks
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(defun gus-cc-style()
  (c-set-style "linux")
  (c-set-offset 'innamespace '0)
  (c-set-offset 'inextern-lang '0)
  (c-set-offset 'inline-open '0)
  (c-set-offset 'label '*)
  (c-set-offset 'case-label '*)
  (c-set-offset 'access-label '/)
  (setq c-basic-offset 2)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
)

(add-hook 'c-mode-hook 'gus-cc-style)
(add-hook 'c++-mode-hook 'gus-cc-style)

;; Compilation output
(setq compilation-scroll-output t)

;; Set visual line mode to compilation buffer.
(add-hook 'compilation-mode-hook
          (lambda () (visual-line-mode 1)))

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(push '("\\.\\(?:frm\\|\\(?:ba\\|cl\\|vb\\)s\\)\\'" . visual-basic-mode)
      auto-mode-alist)

;; Hide DOS cr-characters.
(setq magit-diff-hide-trailing-cr-characters t)
;; SpeedUp magit.
(with-eval-after-load 'magit
  (remove-hook 'magit-status-sections-hook 'magit-insert-untracked-files)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (setq magit-auto-revert-mode nil))

(require 'srefactor)
(require 'srefactor-lisp)

;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++. 
(semantic-mode 1) ;; -> this is optional for Lisp

(require 'hideshow)
(require 'sgml-mode)
(defalias 'xml-mode 'sgml-mode 
    "Use `sgml-mode' instead of nXML's `xml-mode'.")
;; (require 'nxml-mode)

;; (add-to-list 'hs-special-modes-alist
;;              '(nxml-mode
;;                "<!--\\|<[^/>]*[^/]>"
;;                "-->\\|</[^/>]*[^/]>"

;;                "<!--"
;;                sgml-skip-tag-forward
;;                nil))
(add-to-list 'hs-special-modes-alist
             '(sgml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'sgml-mode-hook 'hs-minor-mode)
;; (add-hook 'sgml-mode-hook 'linum-mode)
(add-hook 'sgml-mode-hook (lambda () (visual-line-mode -1)))
(add-hook 'c-mode-hook 'linum-mode)
(add-hook 'c++-mode-hook 'linum-mode)

;; optional key bindings, easier than hs defaults
;; (define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)
(define-key sgml-mode-map (kbd "<C-iso-lefttab>") 'hs-toggle-hiding)

;; Remove doxy-graph from mode-line.
(delight (doxy-graph-mode))

;; No TABS for any mode.
(setq indent-tabs-mode nil)

(defun my-init-perl-mode ()
  (setq perl-indent-level                2)
  (setq indent-tabs-mode nil)
  (setq perl-continued-statement-offset  2)
  (setq perl-continued-brace-offset     -2) )
(my-init-perl-mode)

(provide 'compile-tools)

;;; compile-tools.el ends here
