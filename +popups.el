;;; +popups.el -*- lexical-binding: t; -*-

;; Popup settings

;; Select the IList buffer when it is shown
(after! imenu-list
  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :quit nil :select t :ttl 0))

;; Larger undo tree window
(set-popup-rule! " \\*undo-tree\\*" :slot 2 :side 'left :size 20 :modeline nil :select t :quit t)

;; For better editting org src blocks
(after! org
  (set-popup-rule! "^\\*Org Src" :side 'bottom :slot -2 :height 0.6 :width 0.5 :select t :autosave t :ttl nil :quit nil :select t))

;; (set-popup-rule! "^\\*Ibuffer\\*"  :select t)

;; (after! magit
;;   (set! :popup "^\\(?: ?\\*\\)?magit.*: "
;;     '((slot . -1) (side . right) (size . 80))
;;     '((select . t) (quit . nil)))
;;   (set! :popup "^\\*magit.*popup\\*"
;;     '((slot . 0) (side . right))
;;     '((select . t)))
;;   (set! :popup "^\\(?: ?\\*\\)?magit-revision:.*"
;;     '((slot . 2) (side . right) (window-height . 0.6))
;;     '((select . t)))
;;   (set! :popup "^\\(?: ?\\*\\)?magit-diff:.*"
;;     '((slot . 2) (side . right) (window-height . 0.6))
;;     '((select . nil))))

;; (after! magit
;;   ;(set! :popup "^\\*magit:*" '((slot . -1) (side . right) (size . 60)) '((modeline . nil) (select)))
;;   (set! :popup "^magit:" '((slot . -1) (side . right) (size . 60)) '((modeline . nil) (select)))
;;   (set! :popup "^\\*magit.*popup\\*" '((slot . 1) (side . right) (size . 50)) '((modeline . nil) (select . t)))
;;   (set! :popup "^\\*magit-revision:.*" '((slot . 0) (side . right) (window-height . 0.6)) '((modeline . nil) (select . t)))
;;   (set! :popup "^\\*magit-diff:.*" '((slot . 0) (side . right) (window-height . 0.6)) '((modeline . nil) (select . nil)))
;;   (set! :popup "^magit-log:.*" '((vslot . 1) (side . right) (size . 130)) nil))

;; (after! ein
;;   (set! :popup "^\\*ein:notebooklist .*" '((side . left) (size . 60)) '((select . t)))
;;   (set! :popup "^\\*ein: .*" nil '(modeline . t)))

