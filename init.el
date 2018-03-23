;;; config/boy/init.el -*- lexical-binding: t; -*-

;; Prevents the unstyled mode-line flash at startup
(setq-default mode-line-format nil)

(setq
      doom-font (font-spec :family "Fira Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Fira Mono" :size 19))
