;;; ~/emacs/doom_private/+term.el -*- lexical-binding: t; -*-

(setq eshell-hist-ignoredups t
      eshell-cmpl-cycle-completions nil
      eshell-cmpl-ignore-case t)

(defun +boy/eshell-gotoend-or-send ()
    "In eshell, either go to the end of the buffer or, if point
    is anywherein the last line, send the user input."
    (interactive)
    ;; Check if we are in the last line or not
    (let ((last-line-p (save-excursion
                         (end-of-line)
                         (eobp))))
      (if last-line-p
          (eshell-send-input)
        (goto-char (point-max)))))
