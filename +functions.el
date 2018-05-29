;;; +functions.el --- My Own Awesome™ Functions

;; Scroll and recenter
;; (defun +boy/down-scroll ()
;;   "Scroll down and center the screen"
;;   (interactive)
;;   (forward-line)
;;   (recenter))

;; ;; Smooth scroll up
;; (defun +boy/up-scroll ()
;;   "Scroll up and center the screen"
;;   (interactive)
;;   (previous-line 1 1)
;;   (recenter))

(defun +boy/up-scroll (n)
  "Scroll up marker and line N times."
  (interactive "p")
  (line-move (* -1 n))
  (unless (eq (window-start) (point-min))
    (scroll-down n)))

(defun +boy/down-scroll (n)
  "Scroll down marker and line N times."
  (interactive "p")
  (line-move n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))

;; Delete a word forward without pasting in the kill-region
(defun +boy/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

;; Delete a word backwards without modifying the kill-region
(defun +boy/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (+boy/delete-word (- arg)))

(defun +boy/kill-line ()
  "Like kill-line but without adding anything to the kill ring."
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (if kill-whole-line (delete-char 1)))

; Functions to easily toggle the recording of macros.
(defun +boy/macro-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys)
    '+boy/macro-off)
  (start-kbd-macro nil))

(defun +boy/macro-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys)
    '+boy/macro-on)
  (end-kbd-macro))
