;;; ~/.doom.d/+feature.el -*- lexical-binding: t; -*-

;; Let flycheck search for required files in the `load-path' and the current folder.
(setq flycheck-emacs-lisp-load-path '("./"))

;; Choose hunspell as our spell checker
(setq ispell-program-name "hunspell")
