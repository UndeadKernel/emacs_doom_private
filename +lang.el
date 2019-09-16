;;; ~/.doom.d/+lang.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure-mode")))
