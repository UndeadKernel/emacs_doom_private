;;; +vanilla.el -*- lexical-binding: t; -*-

;; Smooth mouse scrolling
(setopt mouse-wheel-scroll-amount '(2 ((shift) . 1))  ; scroll two lines at a time
      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
      mouse-wheel-follow-mouse t                    ; scroll window under mouse
      scroll-step 1)

;; Backups
(setopt make-backup-files t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Favor visual line wrapping
(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; Window splitting logic
(setopt window-combination-resize t ; after splitting, rebalance windows
      window-combination-limit nil)

;; Scrolling commands do not cancel isearch
(setopt isearch-allow-scroll t)

;; Pop mark improvements (http://endlessparentheses.com/faster-pop-to-mark-command.html)
;; pop mark as in C-u C-SPC C-SPC C-SPC ...
(setopt set-mark-command-repeat-pop t)
;; Pop until marker actually moves
(advice-add 'pop-to-mark-command :around #'+boy/multi-pop-to-mark)

;; Enable narrow to region
(put 'narrow-to-region 'disabled nil)

;; Change re syntax to a more friendly one
(after! re-builder
  (setopt reb-re-syntax 'string))
