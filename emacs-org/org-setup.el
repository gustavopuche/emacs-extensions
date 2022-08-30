(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

;; (setq org-odt-styles-file "/home/gustavo/org/styles.odt")
;; (require 'ox-org)
(require 'org-brain)

;; (require 'ox-koma-letter)

;; (eval-after-load 'ox-koma-letter
;;   '(progn
;;      (add-to-list 'org-latex-classes
;;                   '("my-letter"
;;                     "\\documentclass\{scrlttr2\}
;;      \\setkomavar{frombank}{(1234)\\,567\\,890}
;;      \[DEFAULT-PACKAGES]
;;      \[PACKAGES]
;;      \[EXTRA]"))

;;      (setq org-koma-letter-default-class "my-letter")))

;; ;; Setup org minted
;; (setq org-latex-listings 'minted
;;       org-latex-packages-alist '(("newfloat" "minted"))
;;       org-latex-pdf-process
;;       '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;; 	"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; (setq org-latex-minted-options '(("breaklines")("fontsize" "\\tiny")("tabsize" "2")("frame" "lines")("autogobble")))

;; ;; Setup org beamer tikzpicture
;; (add-to-list 'org-latex-packages-alist
;;              '("" "tikz" t))

;; (eval-after-load "preview"
;;   '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

;; (setq org-latex-to-pdf-process "lualatex --shell-escape --batch %f")

;; (setq org-latex-create-formula-image-program 'imagemagick)

;; (setq org-latex-inline-image-rules '(("file" . "\\.\\(pdf\\|jpeg\\|jpg\\|png\\|ps\\|eps\\|tikz\\|pgf\\|svg\\|gif\\)\\'")))

;; visual-line-mode active with latex-mode
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-brain-visualize-text-hook 'visual-line-mode)

;; this controls the color of bold, italic, underline, verbatim, strikethrough
(setq org-emphasis-alist
  '(("*" (bold :foreground "orange" )) ;; this make bold both italic and bold, but not color change
    ("/" (italic :foreground "dark salmon" )) ;; italic text, the text will be "dark salmon"
    ("_" (italic :foreground "DarkViolet") ) ;; underlined text, color is "DarkViolet"
    ("=" (bold :foreground "DeepSkyBlue" )) 
    ("~" (bold :foreground "MediumSeaGreen" ))
    ("+" (bold :foreground "tomato" ))))
(setq org-hide-emphasis-markers t) ;; hides the emphasis markers

;; Add organizer.org file to agenda.
(org-agenda-files '("~/org/brain/organizer.org"))

(provide 'org-setup)
