(define-minor-mode numbers-color-mode
  "Minor mode for highlighting numbers."
  :group 'smilingbuddha
  :lighter " :)"
  (cond
    (numbers-color-mode
      (when (not (or font-lock-mode global-font-lock-mode))
        (font-lock-mode 1))
      (font-lock-add-keywords nil
        (list (list "\\([0-9]+\\)" '(0 'smilingbuddha-pink-face t)) ))
      (message "Turned ON `numbers-color-mode`."))
    ((not numbers-color-mode)
      (font-lock-remove-keywords nil
        (list (list "\\([0-9]+\\)" '(0 'smilingbuddha-pink-face t)) ))
      (font-lock-fontify-buffer)
      (message "Turned OFF `numbers-color-mode`."))))

(defgroup smilingbuddha nil
  "Highlight numbers."
  :version "0.1"
  :group 'smilingbuddha)

(defface smilingbuddha-pink-face
  '((t (:foreground "black" :background "pink")))
  "Face for `smilingbuddha-pink-face`."
  :group 'smilingbuddha)

(defun turn-on-numbers-color-mode ()
(interactive)
  (numbers-color-mode 1))

(defun turn-off-numbers-color-mode ()
(interactive)
  (numbers-color-mode -1))

(define-globalized-minor-mode global-numbers-color-mode
  numbers-color-mode turn-on-numbers-color-mode)
;; (global-numbers-color-mode)
