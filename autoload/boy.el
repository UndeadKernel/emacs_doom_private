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

;;;###autoload
(defun +boy/new-buffer ()
  (interactive)
  (let ((buffer (generate-new-buffer "*new*")))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (funcall (default-value 'major-mode)))))

;;;###autoload
(defun +boy/popup-diagnose ()
  (interactive)
  (message "%s"
           (cl-loop with bname = (buffer-name)
                    for (pred . action) in display-buffer-alist
                    if (and (functionp pred) (funcall pred bname action))
                    return (cons pred action)
                    else if (and (stringp pred) (string-match-p pred bname))
                    return (cons pred action))))

;;;###autoload
(defun +boy/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
