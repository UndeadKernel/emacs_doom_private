;;; private/boy/autoload/boy.el -*- lexical-binding: t; -*-

;; Copy a paragraph and remove all extra spaces and line ends
;;;###autoload
(defun +boy/copy-paragraph (&optional beg end)
  "Save the current region (or line) to the `kill-ring' after stripping extra whitespace and new lines"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((my-text (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert my-text)
      (goto-char 1)
      (while (looking-at "[ \t\n]")
        (delete-char 1))
      (let ((fill-column 9333999))
        (fill-region (point-min) (point-max)))
      (kill-region (point-min) (point-max)))))

;;;###autoload
(defun +boy/unfill-paragraph ()
  "Transform a paragraph into a single line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil t)))
