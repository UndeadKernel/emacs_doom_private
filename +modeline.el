;;; ../../../doom_private/+modeline.el -*- lexical-binding: t; -*-

;; Modeline settings

;; (def-modeline-segment! tiny-bar
;;   "The bar regulates the height of the mode-line in GUI Emacs.
;; Returns \"\" to not break --no-window-system."
;;   (if (display-graphic-p)
;;       (+doom-modeline--make-xpm
;;        (face-background (if (active)
;;                             'doom-modeline-bar
;;                           'doom-modeline-inactive-bar)
;;                         nil t)
;;        14
;;        +doom-modeline-bar-width)
;;     ""))

;; ; Set the modeline of popups as an empty dark line with a tiny blue strip.
;; (def-modeline! popup (tiny-bar ) nil)
;; (map-put +popup-default-parameters 'modeline 'popup)
