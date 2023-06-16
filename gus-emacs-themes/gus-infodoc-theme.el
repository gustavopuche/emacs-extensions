;;; gus-infodoc-theme.el --- infodoc theme

;; Copyright (C) 2000 by Frederic Giroud
;; Copyright (C) 2013 by Syohei YOSHIDA
;; Copyright (C) 2023 by Gustavo Puche

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/emacs-jp/replace-colorthemes
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Port of infodoc theme from `color-themes'

;;; Code:

(deftheme gus-infodoc
  "gus infodoc theme")

(let* ((class '((class color) (min-colors 89)))
       (256color (eq (display-color-cells (selected-frame)) 256))
       (selection "#ff8836")
       (white "#ffffff")
       )

(custom-theme-set-faces
 'gus-infodoc

 '(default ((t (:background "wheat" :foreground "black" :family "MSX Screen 0" :foundry "PfEd" :slant normal :weight normal :height 83 :width normal))))
 '(cursor ((t (:foregound "red"))))
 '(border ((t (:foregound "black"))))
 `(region ((,class (:foregound ,white :background ,selection))))

 '(hi-yellow ((t (:background "yellow" :foreground "black"))))
 '(hi-pink ((t (:background "pink" :foreground "black"))))
 '(hi-green ((t (:background "green" :foreground "black"))))
 '(hi-blue ((t (:background "cyan" :foreground "black"))))
 
 '(blue ((t (:bold t :foreground "blue"))))
 '(bold ((t (:background "wheat" :foreground "black"))))
 '(bold-italic ((t (:bold t :background "wheat" :foreground "black"))))
 '(border-glyph ((t (:bold t))))
 '(calendar-today-face ((t (:underline t :bold t))))
 '(custom-button-face ((t (nil))))
 '(custom-changed-face ((t (:bold t :background "blue" :foreground "white"))))
 '(custom-documentation-face ((t (:bold t :background "wheat" :foreground "purple4"))))
 '(custom-face-tag-face ((t (:underline t :bold t))))
 '(custom-group-tag-face ((t (:underline t :bold t :background "wheat" :foreground "blue"))))
 '(custom-group-tag-face-1 ((t (:underline t :bold t :background "wheat" :foreground "red"))))
 '(custom-invalid-face ((t (:bold t :background "red" :foreground "yellow"))))
 '(custom-modified-face ((t (:bold t :background "blue" :foreground "white"))))
 '(custom-rogue-face ((t (:bold t :background "black" :foreground "pink"))))
 '(custom-saved-face ((t (:underline t :bold t))))
 '(custom-set-face ((t (:bold t :background "white" :foreground "blue"))))
 '(custom-state-face ((t (:bold t :background "wheat" :foreground "dark green"))))
 '(custom-variable-button-face ((t (:underline t))))
 '(custom-variable-tag-face ((t (:underline t :bold t :background "wheat" :foreground "blue"))))
 '(diary-face ((t (:bold t :foreground "red"))))
 '(display-time-mail-balloon-enhance-face ((t (:bold t :background "wheat" :foreground "black"))))
 '(display-time-mail-balloon-gnus-group-face ((t (:bold t :background "wheat" :foreground "blue"))))
 '(display-time-time-balloon-face ((t (:bold t :background "light salmon" :foreground "dark green"))))
 '(font-lock-comment-face ((t (:bold t :background "wheat" :foreground "turquoise4"))))
 '(font-lock-doc-string-face ((t (:bold t :background "wheat" :foreground "purple4"))))
 '(font-lock-function-name-face ((t (:bold t :background "wheat" :foreground "blue4"))))
 '(font-lock-keyword-face ((t (:bold t :background "wheat" :foreground "dark orchid"))))
 '(font-lock-preprocessor-face ((t (:bold t :background "wheat" :foreground "orchid4"))))
 '(font-lock-reference-face ((t (:bold t :background "wheat" :foreground "red3"))))
 '(font-lock-string-face ((t (:bold t :background "wheat" :foreground "dark goldenrod"))))
 '(font-lock-type-face ((t (:bold t :background "wheat" :foreground "brown"))))
 '(font-lock-variable-name-face ((t (:bold t :background "wheat" :foreground "chocolate"))))
 '(font-lock-warning-face ((t (:bold t :background "wheat" :foreground "black"))))
 '(gdb-arrow-face ((t (:bold t :background "LightGreen" :foreground "black"))))
 '(green ((t (:bold t :foreground "green"))))
 '(gui-button-face ((t (:bold t :background "wheat" :foreground "red"))))
 '(gui-element ((t (:bold t :background "wheat" :foreground "black"))))
 `(highlight ((,class (:foregound ,white :background ,selection))))
 '(holiday-face ((t (:bold t :background "pink" :foreground "black"))))
 '(hproperty:but-face ((t (:bold t :background "wheat" :foreground "medium violet red"))))
 '(hproperty:flash-face ((t (:bold t :background "wheat" :foreground "gray80"))))
 '(hproperty:highlight-face ((t (:bold t :background "wheat" :foreground "red"))))
 '(hproperty:item-face ((t (:bold t))))
 '(isearch ((t (:bold t :background "pale turquoise" :foreground "blue"))))
 '(italic ((t (:bold t :background "wheat" :foreground "black"))))
 '(left-margin ((t (:bold t :background "wheat" :foreground "black"))))
 '(list-mode-item-selected ((t (:bold t :background "gray68" :foreground "black"))))
 '(message-cited-text ((t (:bold t :background "wheat" :foreground "brown"))))
 '(message-header-contents ((t (:bold t :background "wheat" :foreground "black"))))
 '(message-headers ((t (:bold t :background "wheat" :foreground "black"))))
 '(message-highlighted-header-contents ((t (:bold t :background "wheat" :foreground "blue"))))
 '(message-url ((t (nil))))
 '(modeline ((t (:bold t :background "light salmon" :foreground "dark green"))))
 '(modeline-buffer-id ((t (:bold t :background "light salmon" :foreground "blue4"))))
 '(modeline-mousable ((t (:bold t :background "light salmon" :foreground "firebrick"))))
 '(modeline-mousable-minor-mode ((t (:bold t :background "light salmon" :foreground "green4"))))
 '(pointer ((t (:bold t :background "wheat" :foreground "red"))))
 '(primary-selection ((t (:bold t :background "medium sea green"))))
 '(red ((t (:bold t :foreground "red"))))
 '(right-margin ((t (:bold t :background "wheat" :foreground "black"))))
 '(secondary-selection ((t (:bold t :background "paleturquoise" :foreground "black"))))
 '(shell-input-face ((t (:bold t :background "wheat" :foreground "blue"))))
 '(shell-option-face ((t (:bold t :background "wheat" :foreground "turquoise4"))))
 '(shell-output-2-face ((t (:bold t :background "wheat" :foreground "dark goldenrod"))))
 '(shell-output-3-face ((t (:bold t :background "wheat" :foreground "dark goldenrod"))))
 '(shell-output-face ((t (:bold t :background "wheat" :foreground "black"))))
 '(shell-prompt-face ((t (:bold t :background "wheat" :foreground "dark orchid"))))
 '(text-cursor ((t (:bold t :background "red" :foreground "wheat"))))
 '(toolbar ((t (:bold t :background "wheat" :foreground "black"))))
 '(underline ((t (:underline t :bold t :background "wheat" :foreground "black"))))
 '(vertical-divider ((t (:bold t))))
 '(widget-button-face ((t (nil))))
 '(widget-button-pressed-face ((t (:bold t :background "wheat" :foreground "red"))))
 '(widget-documentation-face ((t (:bold t :background "wheat" :foreground "dark green"))))
 '(widget-field-face ((t (:bold t :background "gray85"))))
 '(widget-inactive-face ((t (:bold t :background "wheat" :foreground "dim gray"))))
 '(x-face ((t (:bold t :background "wheat" :foreground "black"))))
 '(yellow ((t (:bold t :foreground "yellow"))))
 '(zmacs-region ((t (:bold t :background "lightyellow" :foreground "darkgreen"))))

 ;; org-extra-emphasis.
 '(org-extra-emphasis ((t ( 
                           :height 1.5
                           :inherit default
                           :weight bold
                           :width normal
                           ))))
 '(org-extra-emphasis-01 ((t ( 
                              :family "CherryBomb"
                              :foreground "#A53E2D"
                              :inherit org-extra-emphasis
                              :underline t
                              ))))
 '(org-extra-emphasis-02 ((t ( 
                              :family "Eater"
                              :foreground "#EEB97E"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-03 ((t ( 
                              :family "Diplomata"
                              :foreground "#683231"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-04 ((t ( 
                              :family "Linux Biolinum Keyboard O"
                              :foreground "#353D8B"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-05 ((t ( 
                              :family "Finger-Maniac" :foundry "FSTR" :slant normal :weight normal
                              :foreground "#54436E"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-06 ((t ( 
                              :family "Fuzzy Bubbles"
                              :foreground "#D65D8F"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-07 ((t ( 
                              :family "Gloria Hallelujah"
                              :foreground "#E5C494"
                              :inherit org-extra-emphasis
                              :weight extra-bold
                              ))))
 '(org-extra-emphasis-08 ((t ( 
                              :family "Henny Penny"
                              :foreground "#0B9FCF"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-09 ((t ( 
                              :family "Humor Sans"
                              :foreground "#C9564C"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-10 ((t ( 
                              :family "Humor Sans"
                              :foreground "#C9564C"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-11 ((t ( 
                              :family "Humor Sans"
                              :foreground "#C9564C"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-12 ((t ( 
                              :family "Humor Sans"
                              :foreground "#C9564C"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-13 ((t ( 
                              :family "Humor Sans"
                              :foreground "#C9564C"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-09 ((t ( 
                              :family "Humor Sans"
                              :foreground "#C9564C"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-14 ((t ( 
                              :family "Humor Sans"
                              :foreground "#C9564C"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-15 ((t ( 
                              :family "Humor Sans"
                              :foreground "#C9564C"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-16 ((t ( 
                              :family "Bitstream Vera Sans Mono"
                              :foreground "#ff0000"
                              :inherit org-extra-emphasis
                              ))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gus-infodoc)

;;; gus-infodoc-theme.el ends here
