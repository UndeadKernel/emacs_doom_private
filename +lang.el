;;; ~/.doom.d/+lang.el -*- lexical-binding: t; -*-

;; we like lsp-ui-sideline now :D
(setq lsp-ui-sideline-enable t
      lsp-enable-symbol-highlighting t
      lsp-ui-sideline-show-hover nil
      lsp-ui-sideline-show-code-actions nil)

;; web-mode config
(setq web-mode-markup-indent-offset 2
      web-mode-sql-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-attr-indent-offset 2
      web-mode-enable-current-element-highlight t)

;; typescript indentation level
(setq-default typescript-indent-level 2)

(after! restclient
  ;; allow restclient queries to start with spaces (so as to be able to indent them)
  (setq restclient-method-url-regexp
   "^[[:blank:]]*\\(GET\\|POST\\|DELETE\\|PUT\\|HEAD\\|OPTIONS\\|PATCH\\) \\(.*\\)$"))
