;;; +functions.el --- My Own Awesome™ Functions

(defun +boy/up-scroll (n)
  "Scroll up marker and line N times."
  (interactive "p")
  (if (= (window-start) (point-min))
      (forward-line (- n))
    (scroll-down n)))

(defun +boy/down-scroll (n)
  "Scroll down marker and line N times."
  (interactive "p")
  (let ((scroll-margin 0))
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

;; "Like kill-line but without adding anything to the kill ring."
(defun +boy/kill-line (&optional arg)
  "Delete the rest of the current line; if no nonblanks there, delete thru newline.
With prefix argument ARG, delete that many lines from point.
Negative arguments delete lines backward.
With zero argument, delete the text before point on the current line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

If `show-trailing-whitespace' is non-nil, this command will just
delete the rest of the current line, even if there are no nonblanks
there.

If option `kill-whole-line' is non-nil, then this command deletes the whole line
including its terminating newline, when used at the beginning of a line
with no argument.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line."
  (interactive "P")
  (delete-region
   (point)
   (progn
     (if arg
         (forward-visible-line (prefix-numeric-value arg))
       (if (eobp)
           (signal 'end-of-buffer nil))
       (let ((end
              (save-excursion
                (end-of-visible-line) (point))))
         (if (or (save-excursion
                   ;; If trailing whitespace is visible,
                   ;; don't treat it as nothing.
                   (unless show-trailing-whitespace
                     (skip-chars-forward " \t" end))
                   (= (point) end))
                 (and kill-whole-line (bolp)))
             (forward-visible-line 1)
           (goto-char end))))
     (point))))

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
