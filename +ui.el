;;; +ui.el -*- lexical-binding: t; -*-

;; Show trailing white spaces
(setq show-trailing-whitespace t)

;; Disable trailing whitespaces in the minibuffer
(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

;; ;; From bitwalker's private config
;; (after! neotree
;;   ;; When switching to a file in the current project, expand the directory
;;   ;; tree to the new file buffer
;;   (add-hook! 'find-file-hook
;;     (if (and (buffer-file-name) (neo-global--window-exists-p))
;;         ;; And only if the file is a child of the current neotree root
;;         (if (neo-global--file-in-root-p (buffer-file-name))
;;             ;; We need to trigger neotree-find then switch back to the buffer we just opened
;;             (save-current-buffer (neotree-find))))))

