;;; ../../../doom_private/+popups.el -*- lexical-binding: t; -*-

;; Popup settings

;; (remove-hook '+popup-display-buffer-actions 'display-buffer-in-side-window)
;; (add-hook '+popup-display-buffer-actions #'+popup-display-buffer t)

(set! :popup "^\\*Ibuffer\\*" nil '((modeline . popup) (select . t)))

(after! magit
  ;(set! :popup "^\\*magit:*" '((slot . -1) (side . right) (size . 60)) '((modeline . nil) (select)))
  (set! :popup "^magit:" '((slot . -1) (side . right) (size . 60)) '((modeline . nil) (select)))
  (set! :popup "^\\*magit.*popup\\*" '((slot . 1) (side . right) (size . 50)) '((modeline . nil) (select . t)))
  (set! :popup "^\\*magit-revision:.*" '((slot . 0) (side . right) (window-height . 0.6)) '((modeline . nil) (select . t)))
  (set! :popup "^\\*magit-diff:.*" '((slot . 0) (side . right) (window-height . 0.6)) '((modeline . nil) (select . nil)))
  (set! :popup "^magit-log:.*" '((vslot . 1) (side . right) (size . 130)) nil))

(after! ein
  (set! :popup "^\\*ein:notebooklist .*" '((side . left) (size . 60)) '((select . t)))
  (set! :popup "^\\*ein: .*" nil '(modeline . t)))

