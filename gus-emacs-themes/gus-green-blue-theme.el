;;; tema-gus-green-blue.el --- gus-orange-yellow-theme

;; Copyright (C) 2001 by Tomas Cerha
;; Copyright (C) 2013 by Syohei YOSHIDA
;; Copyright (C) 2024 by Gustavo Puche

;; Author: Gustavo Puche gustavo.puche@gmail.com;
;; Version: 0.4

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see &lt;http://www.gnu.org/licenses/&gt;.

;;; Commentary:
;;
;; Port of deep-blue theme from `color-themes'
;;
;; Fixed selection background.

;;; Code:

;; Fix rainbow delimiters and other staff.
(require 'doom-themes-base)

;; Setting operators.
(defface font-lock-operator-face
  '((t :foreground "red"
           :background "#f5f5f5"))
  "Basic face for highlighting." :group 'font-lock-faces)

(font-lock-add-keywords 'c++-mode '(("[\*\+=<>\!\|&\-]" 0 'font-lock-operator-face)))
(font-lock-add-keywords 'sql-mode '(("[\*\+\.\:=<>\!\|,&\-]" 0 'font-lock-operator-face)))

(defface font-lock-parenthesis-face
  '((t :foreground "red"
           :background "#f5f5f5"))
  "Basic face for highlighting." :group 'font-lock-faces)

(font-lock-add-keywords 'c++-mode '(("[\(\)]," 0 'font-lock-parenthesis-face)))
(font-lock-add-keywords 'sql-mode '(("[\(\)]" 0 'font-lock-parenthesis-face)))

(defface font-lock-brackets-face
  '((t :foreground "red"
           :background "#f5f5f5"))
  "Basic face for highlighting." :group 'font-lock-faces)

(font-lock-add-keywords 'c++-mode '(("[]\.\:[]" 0 'font-lock-brackets-face)))
(font-lock-add-keywords 'sql-mode '(("[][]" 0 'font-lock-brackets-face)))

(defface font-lock-braces-face
  '((t :foreground "red"
           :background "#f5f5f5"))
  "Basic face for highlighting." :group 'font-lock-faces)

(font-lock-add-keywords 'c++-mode '(("[}{]" 0 'font-lock-braces-face)))

(deftheme gus-green-blue
  "gus-green-blue theme")

(let* ((class '((class color) (min-colors 89)))
       (256color (eq (display-color-cells (selected-frame)) 256))
       (background          "#26cb52")
       (current-line (if 256color "#121212" "#14151E"))
       (block-background (if 256color "#262626" "#1F2232"))
       (aqua                "#00ecff")
       (black               "#000000")
       (blue                "#00ceff")
       (blue-dark           "Deepskyblue2")
       (blue-dark-dark      "royal blue")
       (coral               "coral")
       (cyan                "cyan")
       (green               "#5dfd93")
       (lime                "lime green")
       (magenta             "#a31db1")
       (orange              "#ffaa00")
       (orange-light        "#ffc297")
       (pink                "#ffaaff")
       (pink-dark           "#ff00ff")
       (pink-light          "#ffcdff")
       (purple              "#c397d8")
       (red                 "red")
       (red-pink            "#ff557f")
       (sandy               "sandy brown")
       (turquoise           "pale turquoise")
       (turquoise-dark      "turquoise1")
       (violet              "#b9c6ff")
       (violet-dark         "dark violet")
       (yellow              "#ffff7f")
       (yellow-dark         "yellow")
       (white               "#ffffff")
       (gray                "#5f665d")
       (alert               "red")
       (comment             gray)
       (constant            "lightyellow")
       (current-line-number "#ffd700")
       (cursor              green)
       (directory           red)
       (executable          pink-dark)
       (foreground          white)
       (function            white)
       (inactive            "navajo white")
       (include             pink)
       (keyword             yellow-dark)
       (link                "yellow")
       (modified            "gold")
       (numbers             turquoise)
       (operator            violet-dark)
       (preprocessor        pink)
       (selection           white)
       (string              white)
       (type                pink-light)
       (variable            white)
       (warning             yellow-dark)

       ;; Ediff colors.
       (bg1                 "#292b2e")
       (bg2                 "#212026")
       (bg3                 "#100a14")
       (bg4                 "#0a0814")
       (aqua-bg             "#293235")
       (green-bg            "#293235")
       (red-bg              "#3c2a2c")
       (blue-bg             "#293239")
       (yellow-bg           "#32322c"))

  (custom-theme-set-faces
   'gus-green-blue

   ;; ;; from doom-themes-base
   ;;;; rainbow-delimiters
   ;; `(rainbow-delimiters-depth-1-face ((,class(:foreground ,blue))))
   ;; `(rainbow-delimiters-depth-2-face ((,class(:foreground ,magenta))))
   ;; `(rainbow-delimiters-depth-3-face ((,class(:foreground ,green))))
   ;; `(rainbow-delimiters-depth-4-face ((,class(:foreground ,violet))))
   ;; `(rainbow-delimiters-depth-5-face ((,class(:foreground ,teal))))
   ;; `(rainbow-delimiters-depth-6-face ((,class(:foreground ,blue))))
   ;; `(rainbow-delimiters-depth-7-face ((,class(:foreground ,magenta))))
   ;; `(rainbow-delimiters-depth-8-face ((,class(:foreground ,green))))
   ;; `(rainbow-delimiters-depth-9-face ((,class(:foreground ,violet))))
   ;; `(rainbow-delimiters-base-error-face ((,class(:weight bold :foreground ,red))))
   ;; `(rainbow-delimiters-unmatched-face  ((,class(:foreground ,red :weight bold :inverse-video t))))
   ;; `(rainbow-delimiters-mismatched-face ((,class(:weight bold))))
   ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; All the icons
   `(all-the-icons-blue ((t (:foreground ,blue-dark))))
   `(all-the-icons-green ((t (:foreground ,green))))
   `(all-the-icons-purple ((t (:foreground ,green))))
   
   
   `(default ((t (:background ,background :foreground ,foreground :family "CPMono_v07 Black" :weight light :height 98))))
   `(all-the-icons-purple ((t (:foreground "green"))))
   `(mouse ((t (:foregound ,cursor))))
   `(border ((t (:foregound "black"))))
   `(warning ((t (:foreground ,warning :weight bold))))
   `(link ((t (:foreground ,link :weight bold))))

   `(outline-1 ((t (:foreground ,yellow))))
   `(outline-2 ((t (:foreground ,green))))
   `(outline-3 ((t (:foreground ,orange))))
   `(outline-4 ((t (:foreground ,pink))))
   `(outline-5 ((t (:foreground ,warning))))
   `(outline-6 ((t (:foreground ,red))))
   `(outline-7 ((t (:foreground ,violet))))
   `(outline-8 ((t (:foreground ,constant))))

   `(Info-title-1-face ((t (:bold t :weight bold :height 1.728))))
   `(Info-title-2-face ((t (:bold t :weight bold :height 1.44))))
   `(Info-title-3-face ((t (:bold t :weight bold :height 1.2))))
   `(Info-title-4-face ((t (:bold t :weight bold))))
   `(bold ((t (:bold t :weight bold))))
   `(bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
   `(border ((t (:background "black"))))
   `(calendar-today-face ((t (:background "blue"))))
   `(change-log-acknowledgement-face ((t (:italic t :slant italic :foreground "CadetBlue"))))
   `(change-log-conditionals-face ((t (:foreground ,green))))
   `(change-log-date-face ((t (:foreground "burlywood"))))
   `(change-log-email-face ((t (:foreground ,green))))
   `(change-log-file-face ((t (:bold t :weight bold :foreground "goldenrod"))))
   `(change-log-function-face ((t (:foreground ,green))))
   `(change-log-list-face ((t (:bold t :weight bold :foreground "DeepSkyBlue1"))))
   `(change-log-name-face ((t (:foreground "gold"))))
   `(comint-highlight-input ((t (:bold t :weight bold))))
   `(comint-highlight-prompt ((t (:foreground "white"))))
   `(cursor ((t (:background "green" :foreground "black"))))
   `(cvs-filename-face ((t (:foreground "lightblue"))))
   `(cvs-handled-face ((t (:foreground "pink"))))
   `(cvs-header-face ((t (:bold t :foreground "lightyellow" :weight bold))))
   `(cvs-marked-face ((t (:bold t :foreground "green" :weight bold))))
   `(cvs-msg-face ((t (:italic t :slant italic))))
   `(cvs-need-action-face ((t (:foreground ,orange))))
   `(cvs-unknown-face ((t (:foreground "red"))))
   `(diary-face ((t (:foreground "orange red"))))
   `(diff-added-face ((t (nil))))
   `(diff-changed-face ((t (nil))))
   `(diff-context-face ((t (:foreground "grey70"))))
   `(diff-file-header-face ((t (:bold t :background "grey60" :weight bold))))
   `(diff-function-face ((t (:foreground "grey70"))))
   `(diff-header-face ((t (:background "grey45"))))
   `(diff-hunk-header-face ((t (:background "grey45"))))
   `(diff-index-face ((t (:bold t :weight bold :background "grey60"))))
   `(diff-nonexistent-face ((t (:bold t :weight bold :background "grey60"))))
   `(diff-removed-face ((t (nil))))

   `(font-latex-bold-face ((t (:bold t :foreground "OliveDrab" :weight bold))))
   `(font-latex-italic-face ((t (:italic t :foreground "OliveDrab" :slant italic))))
   `(font-latex-math-face ((t (:foreground "burlywood"))))
   `(font-latex-sedate-face ((t (:foreground "LightGray"))))
   `(font-latex-string-face ((t (:foreground ,string))))
   `(font-latex-warning-face ((t (:bold t :foreground ,pink :weight bold))))

   `(font-lock-builtin-face ((t (:foreground ,yellow))))
   `(font-lock-comment-face ((t (:italic t :foreground ,comment :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,constant :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,comment))))
   `(font-lock-doc-string-face ((t (:foreground ,string))))
   
   `(font-lock-function-name-face ((,class (:bold t :foreground ,function :weight bold))))
   `(font-lock-keyword-face ((t (:bold t :foreground ,keyword :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,preprocessor))))
   `(font-lock-reference-face ((t (:foreground "pink"))))
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-type-face ((t (:foreground ,type))))
   `(font-lock-variable-name-face ((t (:foreground ,variable))))
   `(font-lock-warning-face ((t (:foreground ,warning))))
   `(font-lock-operator-face ((,class(:foreground ,operator :weight bold))))
   `(font-lock-parenthesis-face ((t (:foreground "yellow" :weight bold))))
   `(font-lock-brackets-face ((t (:foreground ,operator :weight bold))))
   `(font-lock-braces-face ((t (:foreground "SteelBlue1" :weight bold))))
   `(fringe ((t (:foreground ,function))))

   ;;;;; ediff
   `(ediff-current-diff-A ((,class(:background ,red-bg :foreground ,red :extend t))))
   `(ediff-current-diff-Ancestor ((,class(:background ,aqua-bg :foreground ,aqua :extend t))))
   `(ediff-current-diff-B ((,class(:background ,green-bg :foreground ,green :extend t))))
   `(ediff-current-diff-C ((,class(:background ,blue-bg :foreground ,blue :extend t))))
   `(ediff-even-diff-A ((,class(:background ,bg3 :extend t))))
   `(ediff-even-diff-Ancestor ((,class(:background ,bg3 :extend t))))
   `(ediff-even-diff-B ((,class(:background ,bg3 :extend t))))
   `(ediff-even-diff-C ((,class(:background ,bg3 :extend t))))
   `(ediff-fine-diff-A ((,class(:background ,red :foreground ,bg1 :extend t))))
   `(ediff-fine-diff-Ancestor ((,class(:background nil :inherit bold :extend t))))
   `(ediff-fine-diff-B ((,class(:background ,green :foreground ,bg1))))
   `(ediff-fine-diff-C ((,class(:background ,blue :foreground ,bg1))))
   `(ediff-odd-diff-A ((,class(:background ,bg4 :extend t))))
   `(ediff-odd-diff-Ancestor ((,class(:background ,bg4 :extend t))))
   `(ediff-odd-diff-B ((,class(:background ,bg4 :extend t))))
   `(ediff-odd-diff-C ((,class(:background ,bg4 :extend t))))
   
   ;;;;; nxml
   `(nxml-attribute-local-name ((,class(:foregound ,orange))))
   `(nxml-attribute-value ((,class(:foreground ,white))))
   `(nxml-attribute-value-delimiter ((,class(:foreground ,purple))))
    
   ;; Emacs interface
   `(cursor ((,class (:background ,cursor))))
   `(fringe ((,class (:foregound ,yellow))))
   `(linum ((,class (:background ,current-line :foreground ,"yellow"))))
   `(border ((,class (:background ,current-line))))
   `(border-glyph ((,class (nil))))
   `(highlight ((,class (:foreground ,"white" :background ,selection))))
   `(match ((,class (:foregound ,white :background ,coral))))
   `(gui-element ((,class (:background ,current-line :foreground ,foreground))))
   `(mode-line ((,class (:foreground nil :background ,current-line
                                     :box (:line-width 2 :color ,foreground)
                                     :family "CPMono_v07" :foundry "PYRS" :slant normal :weight light :height 98 :width normal))))
   `(mode-line-buffer-id ((,class (:foreground ,orange :background nil))))
   `(mode-line-inactive ((,class (:inherit mode-line
                                           :foreground ,inactive
                                           :background ,current-line :weight normal
                                           :box (:line-width 1 :color ,foreground)))))
   `(mode-line-emphasis ((,class (:foreground ,foreground :slant italic))))
   `(mode-line-highlight ((,class (:foreground ,"black" :background ,yellow :box nil))))
   `(header-line ((,class (:foregound ,orange))))
   `(minibuffer-prompt ((,class (:foreground ,yellow))))
   `(region ((,class (:foreground ,"white" :background ,selection))))
   `(secondary-selection ((,class (:background ,current-line))))
   
   `(header-line ((,class (:inherit mode-line :foreground ,purple :background nil))))
   
   `(trailing-whitespace ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-trailing ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-space-after-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-space-before-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-empty ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-line ((,class (:background nil :foreground ,red))))
   `(whitespace-indentation ((,class (:background nil :foreground ,aqua))))
   `(whitespace-space ((,class (:background nil :foreground ,selection))))
   `(whitespace-newline ((,class (:background nil :foreground ,selection))))
   `(whitespace-tab ((,class (:background nil :foreground ,selection))))
   `(whitespace-hspace ((,class (:background nil :foreground ,selection))))

   
   ;; other.
   `(fringe ((t (:background "#405060"))))
   `(header-line ((t (:box (:line-width 2 :style released-button) :background "grey20" :foreground "grey90" :box nil))))
   `(highlight ((t (:background "#feaa51"))))
   `(holiday-face ((t (:foreground "green"))))
   `(info-header-node ((t (:foreground "DeepSkyBlue1"))))
   `(info-header-xref ((t (:bold t :weight bold :foreground ,green))))
   `(info-menu-5 ((t (:foreground "wheat"))))
   `(info-menu-header ((t (:bold t :weight bold))))
   `(info-node ((t (:foreground "DeepSkyBlue1"))))
   `(info-xref ((t (:bold t :foreground ,green :weight bold))))
   `(isearch ((t (:background "turquoise" :foreground "white"))))
   `(isearch-lazy-highlight-face ((t (:foreground "#feaa51" :background "white" :inverse-video t))))
   `(italic ((t (:italic t :slant italic))))
   `(menu ((t (:background "gray" :foreground "black"))))
   `(modeline ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))
   `(modeline-buffer-id ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))
   `(modeline-mousable ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))
   `(modeline-mousable-minor-mode ((t (:background "gray" :foreground "black" :box (:line-width 2 :style released-button)))))
   `(shadow ((t (:foreground ,pink))))


   `(hi-yellow ((t (:background "yellow" :foreground "black"))))
   `(hi-pink ((t (:background "pink" :foreground "black"))))
   `(hi-green ((t (:background "green" :foreground "black"))))
   `(hi-blue ((t (:background "cyan" :foreground "black"))))
   
   ;; `(region ((t (:background "#feaa51"))))
   `(region ((t (:foreground "#feaa51" :inverse-video t))))
   
   `(scroll-bar ((t (:background "gray" :foreground "#506070"))))
   `(secondary-selection ((t (:background "yellow" :foreground "gray10"))))
   `(show-paren-match-face ((t (:bold t :foreground "yellow" :weight bold))))
   `(show-paren-mismatch-face ((t (:bold t :foreground ,orange :weight bold))))
   `(tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
   ;; `(tooltip ((t (:background "lightyellow" :foreground "black"))))

   ;; company.
   `(company-tooltip-selection ((t (:background ,orange :foreground "black"))))

   `(trailing-whitespace ((t (:background "#102e4e"))))
   `(underline ((t (:underline t))))

   `(widget-button-face ((t (:bold t :weight bold))))
   `(widget-button-pressed-face ((t (:foreground ,orange))))
   `(widget-documentation-face ((t (:foreground "lime green"))))
   `(widget-field-face ((t (:background "dim gray"))))
   `(widget-inactive-face ((t (:foreground "light gray"))))
   `(highlight-numbers-number ((t (:foreground ,numbers))))
   `(widget-single-line-field-face ((t (:background "dim gray"))))

   ;; dired.
   `(dired-directory ((t (:bold t :foreground ,yellow :weight bold))))
   `(dired-header ((t (:bold t :foreground ,yellow-dark :weight bold))))
   `(dired-mark ((t (:bold t :foreground ,yellow :weight bold))))
   `(dired-marked ((t (:bold t :foreground ,red :weight bold))))
   `(dired-warning ((t (:bold t :foreground ,yellow :weight bold))))
   
   ;; helm.
   `(helm-M-x-key ((t (:foreground ,keyword :weight bold))))
   `(helm-lsp-diag-warning ((t (:foreground ,warning :weight bold))))
   `(helm-source-header ((t (:foreground ,orange))))
   `(helm-swoop-line-number-face ((t (:foreground ,green))))
   `(helm-etags-file ((t (:foreground ,red))))
   `(helm-gtags-file ((t (:foreground ,red))))
   `(helm-gtags-lineno ((t (:foreground ,white))))
   `(helm-history-remote ((t (:foreground ,pink))))
   `(helm-match ((t (:foreground ,yellow))))
   ;; `(helm-minibuffer-prompt ((t (:foreground ,yellow))))
   `(helm-selection ((t (:background ,orange :foreground ,white))))
   `(helm-selection-line ((t (:background ,yellow :foreground ,black))))
   `(helm-buffer-size ((,class (:foreground ,pink))))
   `(helm-buffer-process ((,class (:foreground ,directory))))
   `(helm-buffer-file ((,class (:foreground ,directory))))
   `(helm-ff-executable ((,class (:foreground ,executable))))
   `(helm-ff-file-extension ((,class (:foreground ,yellow))))
   `(helm-ff-directory ((,class (:foreground ,directory))))
   `(helm-ff-dotted-directory ((,class (:foreground ,directory))))
   `(helm-ff-rsync-progress ((,class (:foreground ,white))))
   `(helm-swoop-target-line-block-face ((,class (:foreground ,"black" :background ,"yellow"))))
   `(helm-swoop-target-line-face ((,class (:background ,"yellow" :foreground ,"black"))))
   `(helm-swoop-target-word-face ((,class (:background ,"yellow" :foreground ,"black"))))

   ;; ibuffer.
   `(ibuffer-locked-buffer ((t (:foreground "green"))))

   ;; magit.
   `(magit-branch-current ((,class (:foreground ,turquoise-dark))))
   `(magit-branch-local ((,class (:foreground ,turquoise-dark))))
   `(magit-branch-remote ((,class (:foreground ,green))))
   `(magit-bisect-good ((,class (:foreground ,green))))
   `(magit-commit ((,class (:foreground ,green))))
   `(magit-log-date ((,class (:foreground ,turquoise-dark))))
   `(magit-process-ok ((,class (:foreground ,green))))
   `(magit-reflog-amend ((,class (:foreground ,turquoise-dark))))
   `(magit-reflog-checkout ((,class (:foreground ,turquoise-dark))))
   `(magit-reflog-commit ((,class (:foreground ,green))))
   `(magit-reflog-merge ((,class (:foreground ,green))))
   `(magit-reflog-other ((,class (:foreground ,turquoise-dark))))
   `(magit-reflog-rebase ((,class (:foreground ,turquoise-dark))))
   `(magit-reflog-remote ((,class (:foreground ,turquoise-dark))))

   `(magit-section-heading ((,class (:foreground ,yellow-dark))))
   
   
   `(magit-diff-file-heading-selection ((,class (:foreground ,yellow))))
   `(magit-diff-hunk-heading ((,class (:foreground ,white))))
   `(magit-diff-hunk-heading-selection ((,class (:foreground ,yellow))))
   `(magit-diffstat-removed ((,class (:foreground ,pink))))
   `(magit-filename ((,class (:foreground ,yellow))))
   `(magit-header-line ((,class (:foreground ,white))))
   `(git-commit-comment-detached ((,class (:foreground ,pink))))
   `(git-commit-comment-branch-local ((,class (:foreground ,white))))
   `(git-commit-comment-heading ((,class (:foreground ,white))))

   ;; Man.
   `(Man-overstrike ((,class (:foreground ,white))))
   `(Man-underline ((t (:foreground ,blue-dark-dark :weight bold))))

   ;; org.
   `(org-macro ((t (:foreground ,orange))))
   `(org-meta-line ((t (:foreground ,yellow))))
   `(org-code ((t (:foreground ,orange))))
   `(org-quote ((t (:foreground ,orange))))
   `(org-document-info-keyword ((t (:foreground ,orange))))
   `(org-document-title ((t (:foreground ,aqua))))
   `(org-table ((t (:foreground ,white))))
   `(org-latex-and-related ((t (:foreground ,orange))))
   `(org-agenda-current-date ((t (:foreground ,yellow))))
   `(org-agenda-current-time ((t (:foreground ,orange))))
   `(org-block ((t (:foreground ,white))))
   `(org-block-begin-line ((t (:foreground ,white :weight bold))))
   `(org-block-end-line ((t (:foreground ,white :weight bold))))
   `(org-table ((t (:foreground ,white :weight bold))))
   `(org-time-grid ((t (:foreground ,red :weight bold))))
   `(org-property-value ((t (:foreground ,yellow))))
   `(org-special-keyword ((t (:foreground ,red :weight bold))))
   `(org-scheduled-today ((t (:foreground ,white :weight bold))))
   `(org-scheduled-previously ((t (:foreground ,red :weight bold))))
   `(org-upcoming-distant-deadline ((t (:foreground ,warning :weight bold))))

   `(org-agenda-date ((t (:foreground ,yellow))))
   `(org-agenda-date-today ((t (:foreground ,green))))
   `(org-agenda-current-time ((t (:foreground ,blue))))
   `(org-warning ((t (:foreground ,warning :weight bold))))

   `(org-headline-done ((t (:foreground ,green))))

   ;; org-brain
   `(org-brain-parent ((t (:foreground ,green))))
   
   ;; org-extra-emphasis.
   ;; !!, !@, !%, !&, @!, @@, @%, @&, %!, %@, %%, %&, &!, &@, &%, &&
   `(org-extra-emphasis ((t ( 
                             :height 1.5
                             :inherit default
                             :weight bold
                             :width normal
                             ))))
   `(org-extra-emphasis-01 ((t ( 
                                :family "Bitstream Vera Sans Mono"
                                :foreground ,red
                                :inherit org-extra-emphasis
                                :weight extra-bold
                                ))))
   `(org-extra-emphasis-02 ((t ( 
                                :family "Bitstream Vera Sans Mono"
                                :foreground ,orange
                                :inherit org-extra-emphasis
                                :weight extra-bold
                                ))))
   `(org-extra-emphasis-03 ((t ( 
                                :family "Bitstream Vera Sans Mono"
                                :foreground ,yellow
                                :inherit org-extra-emphasis
                                :weight extra-bold
                                ))))
   `(org-extra-emphasis-04 ((t ( 
                                :family "Bitstream Vera Sans Mono"
                                :foreground ,green
                                :inherit org-extra-emphasis
                                :weight extra-bold
                                ))))
   `(org-extra-emphasis-05 ((t ( 
                                :family "Bitstream Vera Sans Mono"
                                :foreground ,cyan
                                :inherit org-extra-emphasis
                                ))))
   `(org-extra-emphasis-06 ((t ( 
                                :family "Bitstream Vera Sans Mono"
                                :foreground ,white
                                :inherit org-extra-emphasis
                                ))))
   `(org-extra-emphasis-07 ((t ( 
                                :family "Bitstream Vera Sans Mono"
                                :foreground ,pink
                                :inherit org-extra-emphasis
                                :weight extra-bold
                                ))))
   `(org-extra-emphasis-08 ((t ( 
                                :family "Bitstream Vera Sans Mono"
                                :foreground ,magenta
                                :inherit org-extra-emphasis
                                ))))
   `(org-extra-emphasis-09 ((t ( 
                                :family "Bitstream Vera Sans Mono"
                                :foreground ,violet
                                :inherit org-extra-emphasis
                                ))))
   ;; treemacs.
   `(treemacs-git-modified-face ((,class (:foreground ,modified))))
   `(treemacs-git-ignored-face ((,class (:foreground ,pink))))
   `(treemacs-fringe-indicator-face ((,class (:foreground ,directory))))
   `(error ((,class (:foreground ,alert))))

   ;; line number colors
   `(line-number ((t (:family "CPMono_v07" :98 height))))
   `(line-number-current-line ((,class (:foreground ,current-line-number))))
   
   ;; lsp
   `(lsp-ui-peek-highlight ((,class (:foreground ,black :background ,yellow))))
   `(lsp-face-highlight-read ((,class (:foreground ,black :background ,green))))
   `(lsp-face-highlight-write ((,class (:foreground ,black :background ,red))))
   `(lsp-face-highlight-textual ((,class (:foreground ,black :background ,yellow))))
   `(lsp-headerline ((,class (:inherit mode-line :foreground ,orange :background,black))))
   `(lsp-headerline-breadcrumb-deprecated-face ((,class (:inherit lsp-headerline :foreground ,orange :bold t :weight bold :height 1.20))))
   `(lsp-headerline-breadcrumb-path-error-face ((,class (:inherit lsp-headerline-breadcrumb-deprecated-face))))
   `(lsp-headerline-breadcrumb-path-face ((,class (:inherit lsp-headerline-breadcrumb-deprecated-face))))
   `(lsp-headerline-breadcrumb-path-hint-face ((,class (:inherit lsp-headerline-breadcrumb-deprecated-face))))
   `(lsp-headerline-breadcrumb-path-info-face ((,class (:inherit lsp-headerline-breadcrumb-deprecated-face))))
   `(lsp-headerline-breadcrumb-path-warning-face ((,class (:inherit lsp-headerline-breadcrumb-deprecated-face))))
   `(lsp-headerline-breadcrumb-project-prefix-face ((,class (:inherit lsp-headerline-breadcrumb-deprecated-face))))
   `(lsp-headerline-breadcrumb-separator-face ((,class (:inherit lsp-headerline-breadcrumb-deprecated-face :foreground ,green))))
   `(lsp-headerline-breadcrumb-symbols-error-face ((,class (:inherit lsp-headerline-breadcrumb-deprecated-face :foreground ,yellow))))
   `(lsp-headerline-breadcrumb-symbols-face ((,class (:inherit lsp-headerline-breadcrumb-symbols-error-face))))
   `(lsp-headerline-breadcrumb-symbols-hint-face ((,class (:inherit lsp-headerline-breadcrumb-symbols-error-face))))
   `(lsp-headerline-breadcrumb-symbols-info-face ((,class (:inherit lsp-headerline-breadcrumb-symbols-error-face))))
   `(lsp-headerline-breadcrumb-symbols-warning-face ((,class (:inherit lsp-headerline-breadcrumb-symbols-error-face))))
   `(lsp-headerline-breadcrumb-unknown-project-prefix-face ((,class (:inherit lsp-headerline-breadcrumb-symbols-error-face))))

   ;;flycheck.
   `(flycheck-fringe-warning ((t (:foreground ,yellow :weight bold))))
   `(flycheck-error ((t (:underline (:color ,red :style wave :position wave) :background ,background))))
   `(flycheck-info ((t (:underline (:color ,green :style wave :position wave) :background ,background))))
   `(flycheck-warning ((t (:underline (:color ,warning :style wave :position wave) :background ,background))))

   ;; compilation colors.
   `(compilation-error ((t (:foreground ,alert :weight bold))))
   `(compilation-warning ((t (:foreground ,warning :weight bold))))

   ;; power line
   `(powerline-active2 ((t (:foreground ,green))))

   ;; rainbow-delimiters
   ;; `(rainbow-delimiters-depth-1-face ((,class(:foreground ,blue))))
   `(rainbow-delimiters-depth-2-face ((,class(:foreground ,modified))))
   ;; `(rainbow-delimiters-depth-3-face ((,class(:foreground ,green))))
   ;; `(rainbow-delimiters-depth-4-face ((,class(:foreground ,violet))))
   ;; `(rainbow-delimiters-depth-5-face ((,class(:foreground ,teal))))
   `(rainbow-delimiters-depth-6-face ((,class(:foreground ,violet-dark))))
   ;; `(rainbow-delimiters-depth-7-face ((,class(:foreground ,magenta))))
   ;; `(rainbow-delimiters-depth-8-face ((,class(:foreground ,green))))
   ;; `(rainbow-delimiters-depth-9-face ((,class(:foreground ,violet))))

   
   ;; terminal colors
   `(term-color-blue ((t (:foreground ,yellow))))
   `(term-color-cyan ((t (:foreground ,cyan))))
   `(term-color-green ((t (:foreground ,green))))
   `(term-color-magenta ((t (:foreground ,magenta))))
   `(term-color-red ((t (:foreground ,pink))))

   
   ;; which-hey.
   `(which-key-group-description-face ((t (:foreground ,yellow :weight bold))))

   `(success ((t (:foreground ,green :weight bold))))

   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gus-green-blue)

;;; gus-green-blue-theme.el ends here
