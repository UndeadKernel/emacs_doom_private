;;; private/boy/autoload/boy.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +boy/back-to-indentation ()
  "Move to indentation respecting `visual-line-mode'."
  (if visual-line-mode
      (flet ((beginning-of-line (arg) (beginning-of-visual-line arg)))
        (back-to-indentation))
    (back-to-indentation)))

;;;###autoload
(defun +boy/beginning-of-line (&optional arg)
  "Move to beginning of line respecting `visual-line-mode'."
  (cond
   ((eq major-mode 'org-mode)
    (org-beginning-of-line arg))
   (visual-line-mode
    (beginning-of-visual-line arg))
   (t (beginning-of-line arg))))

;;;###autoload
(defun +boy/move-to-bol ()
  "Move the cursor to the indentation point or, if already there,
to the beginning of the line respecting `visual-line-mode'."
  (interactive)
  (when (= (point) (progn (+boy/back-to-indentation) (point)))
    (+boy/beginning-of-line)))

