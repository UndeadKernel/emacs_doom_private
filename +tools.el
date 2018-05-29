;;; +tools.el --- description -*- lexical-binding: t; -*-

;; Magit config
(after! magit
  ;; Show differences at the word level when a hunk is selected.
  (setq magit-diff-refine-hunk t))
(add-hook! magit-mode (visual-line-mode +1))

;; EIN config
(set! :ein-notebook-dir "~/Documents/CASED/Development")
(add-hook! ein:notebook-multilang-mode
  (map! :map ein:notebook-mode-map
        "M-p" #'+boy/up-scroll
        "M-n" #'+boy/down-scroll))

