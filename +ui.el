;;; +ui.el -*- lexical-binding: t; -*-

;; Modeline config
(setq doom-modeline-icon t
      doom-modeline-major-mode-icon t
      doom-modeline-minor-modes nil
      doom-modeline-enable-word-count t
      doom-modeline-checker-simple-format t
      doom-modeline-persp-name t
      doom-modeline-persp-name-icon t
      doom-modeline-lsp t)

;; Smaller icons so the modeline shows fully
(setq all-the-icons-scale-factor 1.0)

;; Show trailing white spaces
(setq show-trailing-whitespace t)

;; Disable trailing whitespaces in the minibuffer
(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

;; Reuse dired buffers
(put 'dired-find-alternate-file 'disabled nil)

;; Use switch-window more intelligently
;; ... either switch only real buffers or popup buffers
(when (and (modulep! :ui popup) (modulep! :ui window-select +switch-window))
  (defvar +boy--switch-popups nil "Is t when we are switching between popups.")

  (defun +boy--window-list (frame minibuf window)
    "Return a list of windows depending on the variable +BOY-SWITCH-POPUPS.

When +BOY-SWITCH-POPUPS is t, only return popup windows"
    (let ((windows (window-list frame minibuf window)))
      (if (bound-and-true-p +boy--switch-popups)
          (seq-filter #'+popup-window-p windows)
        (seq-remove #'+popup-window-p windows))))

  (defun +boy/switch-window (&optional popups)
    "Trigger switching between popups or normal windows using `switch-window' and
our patches."
    (interactive "P")
    (if popups
        (let ((+boy--switch-popups t))
          (switch-window))
      (switch-window)))

  ;; patched functions of the `switch-window' package

  (el-patch-feature switch-window)
  (after! switch-window
    (el-patch-defun switch-window--list (&optional from-current-window)
      "List windows for current frame.
It will start at top left unless FROM-CURRENT-WINDOW is not nil"
      (let ((relative (or from-current-window
                          switch-window-relative))
            (frames (if (bound-and-true-p switch-window-multiple-frames)
                        (funcall switch-window-frame-list-function)
                      (list (selected-frame)))))
        (cl-loop for frm in (if relative
                                (cons (selected-frame)
                                      (cl-remove (selected-frame) frames))
                              (cl-sort frames
                                       'switch-window--compare-frame-positions))
                 append ((el-patch-swap window-list +boy--window-list)
                         frm nil
                         (unless (and relative
                                      (equal frm (selected-frame)))
                           (frame-first-window frm))))))

    (el-patch-defun switch-window--other-window-or-frame ()
      "If `switch-window-multiple-frames' is set cycle through all visible
windows from all frames. Call `other-window' otherwise."
      (if switch-window-multiple-frames
          (switch-window--select-window (next-window nil nil 'visible))
        (el-patch-swap (other-window 1)
                       (if (bound-and-true-p +boy--switch-popups)
                           (+popup/other)
                         (other-window 1)))))))

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'stack
        highlight-indent-guides-auto-stack-odd-face-perc 60
        highlight-indent-guides-auto-stack-even-face-perc 60
        highlight-indent-guides-auto-stack-character-face-perc 40))
