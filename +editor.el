;;; ~/emacs/doom_private/+editor.el -*- lexical-binding: t; -*-


(after! objed
  ;; Never start objed automatically
  (pushnew! objed-keeper-commands 'org-cycle 'org-todo '+boy/down-scroll '+boy/up-scroll 'recenter-top-bottom))

;; always indent with tab
(setq-default tab-always-indent t)
