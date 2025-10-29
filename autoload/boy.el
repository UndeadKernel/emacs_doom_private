;;; private/boy/autoload/boy.el -*- lexical-binding: t; -*-

;; Show all non-printable characters in a buffer with overlays.
;;;###autoload
(defun +boy/show-nonascii (&optional all remove)
  "Toggle display of zero-width non-ascii characters in current buffer.
With prefix \\[universal-argument] REMOVE, remove displays.
With prefix \\[universal-argument] \\[universal-argument] ALL show all non-ascii chars."
  (interactive (list (eq 4 (prefix-numeric-value current-prefix-arg))
                     (eq 16 (prefix-numeric-value current-prefix-arg))))
  (if remove (remove-overlays (point-min) (point-max) 'nonascii t)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[[:nonascii:]]" nil t)
        (when (or all (zerop (char-width (char-after (match-beginning 0)))))
          (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put ov 'display
                         (buttonize (format "(%d)" (char-after (match-beginning 0)))
                                    (let ((pos (match-beginning 0)))
                                      (lambda (_) (describe-char pos)))))
            (overlay-put ov 'face 'font-lock-warning-face)
            (overlay-put ov 'nonascii t)))))))

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
    (dotimes (_i 10)
      (when (= p (point))
        (apply orig-fun args)))))

;;;###autoload
(defun +boy/append-font-prop (str prop value)
  "Return a copy of STR with the font property PROP with VALUE appended."
  ;; Get the string's face property
  (let ((current-face (get-text-property 0 'face str)))
    ;; Create a new ring that includes the existing face with the new prop.
    (propertize str 'face (append (if (listp current-face)
                                      current-face
                                    (list current-face))
                                  `(,prop ,value)))))

;;;###autoload
(defun +boy/get-char-pixel-width (str)
  "Return the size of the first character in pixels contained in STR."
  (let ((str-font (font-at 0 nil str)))
    (aref (aref (font-get-glyphs str-font 0 1 str) 0) 4)))

;;;###autoload
(defun +boy/match-strings-widths (str1 str2 pad_char)
  ;; Return STR2 with PAD_CHAR as passing to match the width in pixels of STR1.
  (let ((str2_clone (copy-sequence str2))
        (str1_len (length str1))
        (str2_len (length str2))
        (str1_char_px_width (+boy/get-char-pixel-width str1))
        (str2_char_px_width (+boy/get-char-pixel-width str2))
        (str2_props (get-text-property 0 'face str2)))
    ;; Remove font properties of the second string's clone (do not affect the original string).
    (remove-text-properties 0 str2_len '(face) str2_clone)
    ;; Calculate how many characters extra are needed and add the rest as
    ;; ... padding. Add the face of STR2 to the result.
    (propertize
     (concat str2_clone
             (make-string (- (* (/ str1_char_px_width str2_char_px_width) str1_len ) str2_len) pad_char))
     'face str2_props)))
