;;; +checkers.el -*- lexical-binding: t; -*-

(after! flycheck
  ;; Change the default placement of the flycheck "popup"
  (setopt flycheck-posframe-position 'point-bottom-left-corner
          flycheck-posframe-border-width 4)
  ;; Let flycheck search for required files in the `load-path' and the current folder.
  (setopt flycheck-emacs-lisp-load-path '("./")))
