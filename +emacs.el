;;; +emacs.el -*- lexical-binding: t; -*-

;; Don't save undo history
(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

