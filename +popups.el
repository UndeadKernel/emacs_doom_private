;;; +popups.el -*- lexical-binding: t; -*-

;; Popup settings

;; Select the IList buffer when it is shown
(after! imenu-list
  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :quit nil :select t :ttl 0))

;; Larger undo tree window
(after! undo-tree
  (set-popup-rule! " \\*undo-tree\\*" :slot 2 :side 'left :size 60 :modeline nil :select t :quit t))

;; Larger org src edit
(after! org
  (set-popup-rule! "^\\*Org Src" :side 'bottom :slot -2 :height 0.6 :width 0.5 :select t :autosave t :ttl nil :quit nil))

;; Larger customize popups
(set-popup-rule! "\\*Customize.*:.*" :side 'right :size 100 :select t :quit t)

;; Taller reference window for RefTeX
(set-popup-rule! "\\*RefTeX Select\\*" :size 80)

;; (after! pdf-tools
;;   (setq tablist-context-window-display-action
;;         '((+popup-display-buffer-stacked-side-window)
;;           (side . right)
;;           (slot . 2)
;;           (window-height . 0.4)
;;           (inhibit-same-window . t))
;;         pdf-annot-list-display-buffer-action
;;         '((+popup-display-buffer-stacked-side-window)
;;           (side . right)
;;           (slot . 3)
;;           (inhibit-same-window . t))))
