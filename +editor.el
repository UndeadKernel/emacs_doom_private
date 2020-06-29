;;; ~/emacs/doom_private/+editor.el -*- lexical-binding: t; -*-


(after! objed
  ;; Never start objed automatically
  (pushnew! objed-keeper-commands 'org-cycle 'org-todo))
