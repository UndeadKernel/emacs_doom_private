;;; +completion.el -*- lexical-binding: t; -*-

(after! company
  (setq company-idle-delay nil))

(after! ivy
  ;; Add a kill action to Ivy's buffer switching
  (ivy-set-actions 'ivy-switch-buffer '(("k" kill-buffer "kill")))
  ;; Add a kill action to DOOM's buffer switching
  (ivy-set-actions '+ivy/switch-workspace-buffer '(("k" kill-buffer "kill"))))
