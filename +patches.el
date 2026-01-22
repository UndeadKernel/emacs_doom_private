;;; +patches.el -*- lexical-binding: t; -*-

;; Patch to center the screen after using `objed-next-identifier' or `objed-prev-identifier'.
(el-patch-feature objed)
(after! objed

  (el-patch-defun objed-next-identifier ()
    "Activate object with identifier at point."
    (interactive)
    (if (and objed--buffer
             (eq objed--object 'identifier))
          (objed--next-identifier)
          (el-patch-add (recenter-top-bottom '(4)))
      (unless (thing-at-point 'symbol)
        (re-search-forward  "\\_<" nil t))
      (when (objed--init 'identifier)
        (goto-char (objed--beg)))
      (el-patch-add (recenter-top-bottom '(4)))))

  (el-patch-defun objed-prev-identifier ()
    "Activate object with identifier at point."
    (interactive)
    (objed--prev-identifier)
    (el-patch-add (recenter-top-bottom '(4)))))

;; Patch to make the command-log-window respect the variable `command-log-mode-window-font-size'.
;; The original command-log-mode completely ignores this variable (i.e., the variable is never used)
(el-patch-feature command-log-mode)
(after! command-log-mode
  (el-patch-defun  clm/open-command-log-buffer (&optional arg)
    "Opens (and creates, if non-existant) a buffer used for logging keyboard commands.
If ARG is Non-nil, the existing command log buffer is cleared."
    (interactive "P")
    (with-current-buffer
        (setq clm/command-log-buffer
              (get-buffer-create " *command-log*"))
      (text-scale-set (el-patch-swap 1 command-log-mode-window-font-size)))
    (when arg
      (with-current-buffer clm/command-log-buffer
        (erase-buffer)))
    (let ((new-win (split-window-horizontally
                    (- 0 command-log-mode-window-size))))
      (set-window-buffer new-win clm/command-log-buffer)
      (set-window-dedicated-p new-win t))))

;; When switching to the latex output buffer, switch to the buffer.
;; ... Without this patch, the original buffer retains focus.
(el-patch-feature latex)
(after! latex
  (el-patch-defun TeX-recenter-output-buffer (line)
    "Redisplay buffer of TeX job output so that most recent output can be seen.
The last line of the buffer is displayed on line LINE of the window, or
at bottom if LINE is nil."
    (interactive "P")
    (let ((buffer (TeX-active-buffer)))
      (if buffer
          (let ((old-buffer (current-buffer)))
            (TeX-pop-to-buffer buffer t t)
            (bury-buffer buffer)
            (goto-char (point-max))
            (recenter (if line
                          (prefix-numeric-value line)
                        (/ (window-height) 2)))
            (el-patch-remove (TeX-pop-to-buffer old-buffer nil t)))
        (message "No process for this document.")))))

(el-patch-feature org-modern)
(after! org-modern
  (el-patch-defun org-indent-set-line-properties (level indentation &optional heading)
    "Set prefix properties on current line an move to next one.

LEVEL is the current level of heading.  INDENTATION is the
expected indentation when wrapping line.

When optional argument HEADING is non-nil, assume line is at
a heading.  Moreover, if it is `inlinetask', the first star will
have `org-warning' face."
    (let* ((line (aref (pcase heading
                         (`nil org-indent--text-line-prefixes)
                         (`inlinetask org-indent--inlinetask-line-prefixes)
                         (_ org-indent--heading-line-prefixes))
                       level))
           (wrap
            (org-add-props
                (concat line
                        (if heading (concat (make-string level ?*) " ")
                          (make-string indentation ?\s)))
                nil 'face 'org-indent)))
      (el-patch-add
        ;; If we are in an hline of a table, change the line and wrap prefix to
        ;; ... match the width of the string with the indent.
        (when (+boy/org-table-hline-p)
          (setq line (+boy/match-strings-widths
                      line
                      (+boy/append-font-prop line :height org-modern-table-horizontal)
                      ?\s))
          (setq wrap (+boy/match-strings-widths
                      wrap
                      (+boy/append-font-prop wrap :height org-modern-table-horizontal)
                      ?\s))))
      ;; Add properties down to the next line to indent empty lines.
      (add-text-properties (line-beginning-position) (line-beginning-position 2)
                           `(line-prefix ,line wrap-prefix ,wrap)))
    (forward-line)))
