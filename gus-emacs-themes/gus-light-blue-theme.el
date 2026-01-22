;;; gus-light-blue-theme.el --- Custom theme for faces

;; Copyright (C) 2011-2020 Free Software Foundation, Inc.

;; Author: Gustavo Puche <gustavo.puche@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extracted from the settings in oneonone.el by Drew Adams.

;;; Code:

(deftheme gus-light-blue
  "Face colors utilizing a light blue background.")

(let* ((class '((class color) (min-colors 89)))
       (256color (eq (display-color-cells (selected-frame)) 256))
       (background          "DodgerBlue")
       (current-line (if 256color "#121212" "#14151E"))
       (block-background (if 256color "#262626" "#1F2232"))
       (aqua		    "#00ecff")
       (black		    "#000000")
       (blue		    "#4f97d7")
       (coral		    "coral")
       (cyan		    "cyan")
       (green		    "chartreuse")
       (lime		    "lime green")
       (magenta		    "#a31db1")
       (orange		    "#ffbb88")
       (pink		    "#ffd2ff")
       (purple		    "#c397d8")
       (red		    "#f2241f")
       (sandy		    "sandy brown")
       (violet		    "#b9c6ff")
       (yellow		    "#ffff7f")
       (yellow-dark	    "yellow")
       (white		    "#ffffff")
       (selection	    "#ffaa00")
       (foreground	    "#eaeaea")
       (comment		    "#d6d8d6")
       (executable	    "#ffa0aa")
       (directory	    "cyan")
       (modified	    "gold")
       (alert		    "light pink")
       (constant	    "lightyellow")
       (keyword		    "cyan")
       (inactive	    "navajo white")
       (function	    "yellow")
       (current-line-number "#ffd700")
       (type pink)
       (warning pink)

       ;; Ediff colors.
       (bg1		    "#292b2e")
       (bg2		    "#212026")
       (bg3		    "#100a14")
       (bg4		    "#0a0814")
       (aqua-bg		    "#293235")
       (green-bg	    "#293235")
       (red-bg		    "#3c2a2c")
       (blue-bg		    "#293239")
       (yellow-bg	    "#32322c"))

  (custom-theme-set-faces
   'gus-light-blue

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class(:foreground ,red))))
   `(rainbow-delimiters-depth-2-face ((,class(:foreground ,orange))))
   `(rainbow-delimiters-depth-3-face ((,class(:foreground ,yellow))))
   `(rainbow-delimiters-depth-4-face ((,class(:foreground ,cyan))))
   `(rainbow-delimiters-depth-5-face ((,class(:foreground ,green))))
   `(rainbow-delimiters-depth-6-face ((,class(:foreground "magenta"))))
   `(rainbow-delimiters-depth-7-face ((,class(:foreground ,coral))))
   `(rainbow-delimiters-depth-8-face ((,class(:foreground ,violet))))
   `(rainbow-delimiters-depth-9-face ((,class(:foreground "wheat"))))
   `(rainbow-delimiters-base-error-face ((,class(:weight bold :foreground ,red))))
   `(rainbow-delimiters-unmatched-face  ((,class(:foreground ,red :weight bold :inverse-video t))))
   `(rainbow-delimiters-mismatched-face ((,class(:weight bold))))
   ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   `(default ((,class (:background "LightBlue" :foreground "black"))))
   `(cursor ((,class (:background "red"))))
   `(fringe ((,class (:background "gray85"))))
   ;; Highlighting faces
   `(highlight ((,class (:background "cyan" :foreground "black"))))
   `(highlight-numbers-number ((,class (:foreground "white"))))
   `(region ((,class (:background "MediumAquamarine"))))
   `(secondary-selection ((,class (:background "white" :foreground "black"))))
   `(isearch ((,class (:background "green" :foreground "Black"))))
   `(lazy-highlight ((,class (:background "dark turquoise"))))
   `(query-replace ((,class (:inherit isearch :background "white" :foreground "black"))))
   `(match ((,class (:background "SkyBlue"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "PaleGoldenrod" :foreground "black" :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:overline "red" :underline "red"))))
   `(mode-line-inactive ((,class (:inherit mode-line :background "LightGray" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))
   ;; Escape and prompt faces
   `(escape-glyph ((,class (:background "gold" :foreground "blue" :box (:line-width 1 :color "blue" :style released-button)))))
   `(homoglyph ((,class (:background "gold" :foreground "blue" :box (:line-width 1 :color "blue" :style released-button)))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#b35caf"))))
   `(font-lock-constant-face ((,class (:foreground "deep pink"))))
   `(font-lock-function-name-face ((,class (:foreground "purple"))))
   `(font-lock-keyword-face ((,class (:foreground "tomato"))))
   `(font-lock-punctuation-face ((,class (:foreground "magenta"))))
   `(font-lock-string-face ((,class (:foreground "light sea green"))))
   `(font-lock-type-face ((,class (:foreground "#ffff33"))))
   `(font-lock-variable-name-face ((,class (:foreground "dodger blue"))))
   `(font-lock-warning-face ((,class (:foreground "orange red" :weight bold))))
   '(hi-yellow ((t (:background "yellow" :foreground "black"))))
   '(hi-pink ((t (:background "pink" :foreground "black"))))
   '(hi-green ((t (:background "green" :foreground "black"))))
   '(hi-blue ((t (:background "DeepSkyBlue" :foreground "black"))))
   '(lsp-face-highlight-read ((t (:background "green" :foreground "black"))))
   '(lsp-face-highlight-textual ((t (:background "yellow" :foreground "black"))))
   '(lsp-face-highlight-write ((t (:background "red" :foreground "black"))))

   ;; Compilation faces
   `(next-error ((,class (:inherit region :background "SkyBlue"))))))

(provide-theme 'gus-light-blue)

;;; gus-light-blue-theme.el ends here
