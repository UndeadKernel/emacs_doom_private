;;; ~/.doom.d/+lang.el -*- lexical-binding: t; -*-

;; lsp-ui-sideline is redundant with eldoc and much more invasive
(setq lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil)

;; web mode indentation level
(setq web-mode-markup-indent-offset 2
      web-mode-sql-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)

;; typescript indentation level
(setq-default typescript-indent-level 2)

(after! restclient
  ;; allow restclient queries to start with spaces (so as to be able to indent them)
  (setq restclient-method-url-regexp
   "^[[:blank:]]*\\(GET\\|POST\\|DELETE\\|PUT\\|HEAD\\|OPTIONS\\|PATCH\\) \\(.*\\)$"))
