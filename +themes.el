;;; ~/emacs/doom_private/+themes.el -*- lexical-binding: t; -*-

;; Main theme
(setq doom-theme 'doom-dracula)

;; All themes are safe to load
(setq custom-safe-themes t)

;; Splash image
(setq +doom-dashboard-banner-dir (concat doom-private-dir "banners/")
      +doom-dashboard-banner-file "black-hole-dracula.png"
      +doom-dashboard-banner-padding '(0 . 1))

;; Change some font weights for the sections in LaTeX
(add-hook! LaTeX-mode
  (set-face-attribute 'font-latex-sectioning-1-face nil :height 1.8 :weight 'bold)
  (set-face-attribute 'font-latex-sectioning-2-face nil :height 1.6)
  (set-face-attribute 'font-latex-sectioning-3-face nil :height 1.3)
  (set-face-attribute 'font-latex-sectioning-4-face nil :height 1.1)
  (set-face-attribute 'font-latex-sectioning-5-face nil :height 1.1))

;; Match the background of latex previews and scale a bit less than the default
(after! preview
  (setq preview-scale 1.2)
  (set-face-attribute 'preview-reference-face nil :background (doom-color 'bg)))

;; Color the border of windows according to the `highlight' color of the doom's theme
(add-hook! 'doom-load-theme-hook
  ;; web-mode tag selected highlight color
  (after! web-mode
    (set-face-attribute 'web-mode-current-element-highlight-face nil
                        :foreground (doom-color 'highlight)
                        :background nil
                        :weight 'bold
                        :underline t))
  ;; Color matching parenthesis with the highlight color
  (set-face-attribute 'show-paren-match nil
                      :foreground (doom-color 'highlight)
                      :background (doom-color 'region))
  ;; A more visible window border
  (set-face-attribute 'vertical-border nil :foreground (doom-color 'highlight))
  ;; Flycheck errors use the color of functions
  (after! flycheck
      (set-face-attribute 'flycheck-error nil
                          :underline `(:color ,(doom-color 'functions)
                                       :style wave)))
  ;; Custom doom-one configuration
  (when (custom-theme-enabled-p 'doom-one)
    (after! org
      ;; Purple boxes for Org BEGIN_SRC and END_SRC
      (set-face-attribute 'org-block-begin-line nil
                          :background "#5c3d5c"
                          :foreground "#a16ba1"
                          :weight 'bold
                          :height 0.9
                          :box '(:line-width 2 :color "#5c3d5c")))))

(add-hook! org-mode
  ;; Document title font
  (set-face-attribute 'org-document-title nil :height 2.0)
  ;; Face of keyword DONE (Green like strings)
  (set-face-attribute 'org-done nil :foreground "#98be65")
  ;; Face of keyword TODO or [ ] (Purple like keywords)
  (set-face-attribute 'org-todo nil :foreground "#c678dd")
  ;; Face of ellipsis symbol (Purple like keywords)
  (set-face-attribute 'org-ellipsis nil :foreground "#c678dd")
  ;; Face of the entire headline of a DONE line
  (set-face-attribute 'org-headline-done nil :foreground 'unspecified))

(after! org-modern
    ;; Face for org-modern timestamps and dates
  (set-face-attribute 'org-modern-date-inactive nil :height (face-attribute 'default :height))
  (set-face-attribute 'org-modern-date-active nil :height (face-attribute 'default :height))
  (set-face-attribute 'org-modern-time-inactive nil :height (face-attribute 'default :height))
  (set-face-attribute 'org-modern-time-active nil :height (face-attribute 'default :height)))
