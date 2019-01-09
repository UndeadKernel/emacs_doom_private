;;; +tools.el --- description -*- lexical-binding: t; -*-

;; Magit config
(add-hook! magit-mode (visual-line-mode +1))

;; EIN config
(setq +ein-notebook-dir "~/Documents/CASED/Development")
(add-hook! ein:notebook-multilang-mode
  (map! :map ein:notebook-mode-map
        "M-p" #'+boy/up-scroll
        "M-n" #'+boy/down-scroll))

