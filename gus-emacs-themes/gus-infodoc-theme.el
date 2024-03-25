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
       (alert "light pink")
       (aqua "#00ecff")
       (background "PeachPuff")
       (black "#000000")
       (blue    "#4f97d7")
       (chartreuse "chartreuse2")
       (comment "#d6d8d6")
       (constant "lightyellow")
       (coral "coral")
       (current-line-number "#ffd700")
       (cyan    "cyan")
       (darkorange "DarkOrange1")
       (deepskyblue "DeepSkyBlue")
       (directory "cyan")
       (executable "#ffa0aa")
       (foreground "#eaeaea")
       (fringe   "magenta")
       (function "DodgerBlue")
       (gold "gold3")
       (green "LimeGreen")
       (inactive "navajo white")
       (keyword "HotPink")
       (lime "lime green")
       (link "#ff5000")
       (magenta "thistle")
       (modified "gold")
       (orange "#ffbb88")
       (org-block-bg "#ffeac9")
       (purple "#c397d8")
       (pink "magenta")
       (red     "#f2241f")
       (sandy "sandy brown")
       (selection "#ffaa00")
       (springgreen "SpringGreen2")
       (string "ForestGreen")
       (turquoise "turquoise3")
       (type "#00b0ff")
       (variable "tomato")
       (violet "#b9c6ff")
       (warning pink)
       (white "#ffffff")
       (yellow  "#ffff7f")
       (yellow-dark "yellow")
       )

  (custom-theme-set-faces
   'gus-infodoc

   `(default ((t (:background ,background :foreground "black" :family "CPMono_v07 Black" :weight light :height 98))))
   `(cursor ((t (:foregound "red"))))
   `(border ((t (:foregound "black"))))
   `(region ((,class (:foregound ,white :background ,selection))))

   `(hi-yellow ((t (:background "yellow" :foreground "black"))))
   `(hi-pink ((t (:background "pink" :foreground "black"))))
   `(hi-green ((t (:background "green" :foreground "black"))))
   `(hi-blue ((t (:background "cyan" :foreground "black"))))
   
   `(blue ((t (:bold t :foreground "blue"))))
   `(bold ((t (:foreground "black"))))
   `(bold-italic ((t (:bold t :foreground "black"))))
   `(border-glyph ((t (:bold t))))
   `(calendar-today-face ((t (:underline t :bold t))))
   `(custom-button-face ((t (nil))))
   `(custom-changed-face ((t (:bold t :background "blue" :foreground "white"))))
   `(custom-documentation-face ((t (:bold t :foreground "purple4"))))
   `(custom-face-tag-face ((t (:underline t :bold t))))
   `(custom-group-tag-face ((t (:underline t :bold t :foreground "blue"))))
   `(custom-group-tag-face-1 ((t (:underline t :bold t :foreground "red"))))
   `(custom-invalid-face ((t (:bold t :background "red" :foreground "yellow"))))
   `(custom-modified-face ((t (:bold t :background "blue" :foreground "white"))))
   `(custom-rogue-face ((t (:bold t :background "black" :foreground "pink"))))
   `(custom-saved-face ((t (:underline t :bold t))))
   `(custom-set-face ((t (:bold t :background "white" :foreground "blue"))))
   `(custom-state-face ((t (:bold t :foreground "dark green"))))
   `(custom-variable-button-face ((t (:underline t))))
   `(custom-variable-tag-face ((t (:underline t :bold t :foreground "blue"))))
   `(diary-face ((t (:bold t :foreground "red"))))
   `(display-time-mail-balloon-enhance-face ((t (:bold t :foreground "black"))))
   `(display-time-mail-balloon-gnus-group-face ((t (:bold t :foreground "blue"))))
   `(display-time-time-balloon-face ((t (:bold t :background "light salmon" :foreground "dark green"))))
   `(font-lock-comment-face ((t (:bold t :foreground "turquoise4"))))
   `(font-lock-doc-string-face ((t (:bold t :foreground "purple4"))))
   `(font-lock-function-name-face ((t (:bold t :foreground ,function))))
   `(font-lock-keyword-face ((t (:bold t :foreground ,keyword))))
   `(font-lock-preprocessor-face ((t (:bold t :foreground "orchid4"))))
   `(font-lock-reference-face ((t (:bold t :foreground "red3"))))
   `(font-lock-string-face ((t (:bold t :foreground ,string))))
   `(font-lock-type-face ((t (:bold t :foreground ,type))))
   `(font-lock-variable-name-face ((t (:bold t :foreground ,variable))))
   `(font-lock-warning-face ((t (:bold t :foreground "black"))))
   `(fringe ((t (:bold t :foreground ,fringe))))
   `(gdb-arrow-face ((t (:bold t :background "LightGreen" :foreground "black"))))
   `(green ((t (:bold t :foreground "green"))))
   `(gui-button-face ((t (:bold t :foreground "red"))))
   `(gui-element ((t (:bold t :foreground "black"))))
   `(highlight ((,class (:foregound ,white :background ,selection))))
   `(holiday-face ((t (:bold t :background "pink" :foreground "black"))))
   `(hproperty:but-face ((t (:bold t :foreground "medium violet red"))))
   `(hproperty:flash-face ((t (:bold t :foreground "gray80"))))
   `(hproperty:highlight-face ((t (:bold t :foreground "red"))))
   `(hproperty:item-face ((t (:bold t))))
   `(isearch ((t (:bold t :background "pale turquoise" :foreground "white"))))
   `(italic ((t (:bold t :foreground "black"))))
   `(left-margin ((t (:bold t :foreground "black"))))
   `(link ((t (:bold t :foreground ,link))))
   `(list-mode-item-selected ((t (:bold t :background "gray68" :foreground "black"))))
   `(message-cited-text ((t (:bold t :foreground "brown"))))
   `(message-header-contents ((t (:bold t :foreground "black"))))
   `(message-headers ((t (:bold t :foreground "black"))))
   `(message-highlighted-header-contents ((t (:bold t :foreground "blue"))))
   `(message-url ((t (nil))))
   `(modeline ((t (:bold t :background "light salmon" :foreground "dark green"))))
   `(modeline-buffer-id ((t (:bold t :background "light salmon" :foreground "blue4"))))
   `(modeline-mousable ((t (:bold t :background "light salmon" :foreground "firebrick"))))
   `(modeline-mousable-minor-mode ((t (:bold t :background "light salmon" :foreground "green4"))))
   `(pointer ((t (:bold t :foreground "red"))))
   `(primary-selection ((t (:bold t :background "medium sea green"))))
   `(red ((t (:bold t :foreground "red"))))
   `(right-margin ((t (:bold t :foreground "black"))))
   `(secondary-selection ((t (:bold t :background "paleturquoise" :foreground "black"))))
   `(shell-input-face ((t (:bold t :foreground "blue"))))
   `(shell-option-face ((t (:bold t :foreground "turquoise4"))))
   `(shell-output-2-face ((t (:bold t :foreground "dark goldenrod"))))
   `(shell-output-3-face ((t (:bold t :foreground "dark goldenrod"))))
   `(shell-output-face ((t (:bold t :foreground "black"))))
   `(shell-prompt-face ((t (:bold t :foreground "dark orchid"))))
   `(text-cursor ((t (:bold t :background "red" :foreground ,background))))
   `(toolbar ((t (:bold t :foreground "black"))))
   `(underline ((t (:underline t :bold t :foreground "black"))))
   `(vertical-divider ((t (:bold t))))
   `(widget-button-face ((t (nil))))
   `(widget-button-pressed-face ((t (:bold t :foreground "red"))))
   `(widget-documentation-face ((t (:bold t :foreground "dark green"))))
   `(widget-field-face ((t (:bold t :background "gray85"))))
   `(widget-inactive-face ((t (:bold t :foreground "dim gray"))))
   `(x-face ((t (:bold t :foreground "black"))))
   `(yellow ((t (:bold t :foreground "yellow"))))
   `(zmacs-region ((t (:bold t :background "lightyellow" :foreground "darkgreen"))))

   ;; org
   `(org-block ((t (:bold t :background ,org-block-bg :foreground ,black))))

   ;; org-extra-emphasis.
   ;; !!, !@, !%, !&, @!, @@, @%, @&, %!, %@, %%, %&, &!, &@, &%, &&
   `(org-extra-emphasis ((t ( 
                             :height 1.0
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
   `(org-extra-emphasis-10 ((t ( 
				:family "Bitstream Vera Sans Mono"
				:foreground ,blue
				:inherit org-extra-emphasis
				))))
   `(org-extra-emphasis-11 ((t ( 
				:family "Bitstream Vera Sans Mono"
				:foreground ,deepskyblue
				:inherit org-extra-emphasis
				))))
   `(org-extra-emphasis-12 ((t ( 
				:family "Bitstream Vera Sans Mono"
				:foreground ,turquoise
				:inherit org-extra-emphasis
				))))
   `(org-extra-emphasis-13 ((t ( 
				:family "Bitstream Vera Sans Mono"
				:foreground ,springgreen
				:inherit org-extra-emphasis
				))))
   `(org-extra-emphasis-09 ((t ( 
				:family "Bitstream Vera Sans Mono"
				:foreground ,chartreuse
				:inherit org-extra-emphasis
				))))
   `(org-extra-emphasis-14 ((t ( 
				:family "Bitstream Vera Sans Mono"
				:foreground ,gold
				:inherit org-extra-emphasis
				))))
   `(org-extra-emphasis-15 ((t ( 
				:family "Bitstream Vera Sans Mono"
				:foreground ,darkorange
				:inherit org-extra-emphasis
				))))
   ;; Marker &&
   `(org-extra-emphasis-16 ((t ( 
				:family "Bitstream Vera Sans Mono"
				:foreground ,black
				:inherit org-extra-emphasis
				))))))


;;;###autoload
(when load-file-name
  (add-to-list `custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme `gus-infodoc)

;;; gus-infodoc-theme.el ends here
