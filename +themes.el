;;; ~/emacs/doom_private/+themes.el -*- lexical-binding: t; -*-

;; All themes are safe to load
(setq custom-safe-themes t)

;; Change some font weights for the sections in LaTeX
(add-hook! LaTeX-mode
  (set-face-attribute 'font-latex-sectioning-1-face nil :height 1.6 :weight 'bold)
  (set-face-attribute 'font-latex-sectioning-2-face nil :height 1.2)
  (set-face-attribute 'font-latex-sectioning-3-face nil :height 1.0)
  (set-face-attribute 'font-latex-sectioning-4-face nil :height 0.9)
  (set-face-attribute 'font-latex-sectioning-5-face nil :height 0.9))

;; Purple boxes for Org BEGIN_SRC and END_SRC
(add-hook! org-mode
  ;; Face of keyword DONE (Green like strings)
  (set-face-attribute 'org-done nil :foreground "#98be65")
  ;; Face of keyword TODO or [ ] (Purple like keywords)
  (set-face-attribute 'org-todo nil :foreground "#c678dd")
  ;; Face of ellipsis symbol (Purple like keywords)
  (set-face-attribute 'org-ellipsis nil :foreground "#c678dd")
  ;; Face of the entire headline of a DONE line
  (set-face-attribute 'org-headline-done nil :foreground nil)
  (cond 
   ;; If doom-one theme is enabled
   ((custom-theme-enabled-p 'doom-one)
      ;; Change the style of the BEGIN_SRC and RESULT blocks
      (set-face-attribute 'org-block-begin-line nil
                          :background "#5c3d5c"
                          :foreground "#83898d"
                          :height 0.9
                          :box '(:line-width 2 :color "#5c3d5c")))
   ;; For other themes, disable the changes
   (t
      (set-face-attribute 'org-block-begin-line nil
                          :background unspecified
                          :height uncpecified
                          :box unspecified
                          :inherit org-meta-line))))
