;;; +popups.el -*- lexical-binding: t; -*-

;; Popup settings

;; Select the IList buffer when it is shown
(after! imenu-list
  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :quit nil :select t :ttl 0))

;; Larger undo tree window
(after! undo-tree
  (set-popup-rule! " \\*undo-tree\\*" :slot 2 :side 'right :size 20 :modeline nil :select t :quit t))

;; For better editting org src blocks
(after! org
  (set-popup-rule! "^\\*Org Src" :side 'bottom :slot -2 :height 0.6 :width 0.5 :select t :autosave t :ttl nil :quit nil)

  (set-popup-rules! '(("^\\*Python:.*"    :slot 0 :side 'right :size 40 :select nil :quit nil :transient nil)
                      ("^\\*Python"       :slot 0 :side 'right :size 40 :select nil :quit nil :ttl nil)
                      ("^\\*ob-ipython.*" :slot 2 :side 'right :size 40 :height 0.2 :select nil :quit nil :transient nil :ttl nil))))

;; (set-popup-rule! "^\\*Python:ob-ipython.*" :side 'left :slot 0 :size 80 :select nil :ttl nil :quit nil)

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

