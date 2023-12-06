;;; modules/private/boy/+latex.el -*- lexical-binding: t; -*-

  ;; Choose pdf-tools as the first viewer to choose if available
(setq +latex-viewers '(pdf-tools okular))
(setq +latex-bibtex-file "/home/boy/Documents/CASED/Papers/bib/TK.bib")

(after! latex
  ;; Save without asking when invoking TeX commands
  (setq TeX-save-query nil)
  ;; While inserting commands in comment sections, do not be intelligent and comment the command
  (setq LaTeX-insert-into-comments nil)
  ;; if the babel language is german, set the quotes as if english
  (add-hook 'TeX-language-de-hook
            (lambda ()
              (setq TeX-quote-language `("ngerman" "``" "''" ,TeX-quote-after-quote))))
  ;; Allow `TeX-view' to refresh a PDF in another frame
  ;; https://emacs.stackexchange.com/questions/55395/auctex-and-pdf-tools-in-2-separate-frames-for-dual-monitor-setup
  (advice-add 'TeX-pdf-tools-sync-view :around #'+boy/display-buffer-use-some-frame)
  (advice-add 'pdf-sync-backward-search-mouse :around #'+boy/display-buffer-use-some-frame))

;; Do not spellcheck latex documents when opened, this takes a lot of time.
;;(remove-hook 'flyspell-mode-hook #'+spellcheck|immediately)
(after! tex
  (setq-hook! 'TeX-mode-hook +flyspell-immediately nil))

;; Enable whitespace mode with latex
;; (add-hook 'LaTeX-mode-hook #'whitespace-mode)

;; Enable LaTeX-math-mode by default: add math symbols with the key `LaTeX-math-abbrev-prefix'
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook! LaTeX-mode
  ;; Set the fill column to something large so that we can fold without problems
  ;; whitespace-mode builds wrong regexps if this value is too big
  (set-fill-column 2000)
  ;; In LaTeX mode, insert \( \) instead of $$ when pressing "$"
  (set (make-variable-buffer-local 'TeX-electric-math) (cons "\\(" "\\)")))

(after! smartparens
  ;; Smartparens for whatever reason treats the insertion of dollar signs and quotes as single characters.
  (setq sp--special-self-insert-commands (delete `TeX-insert-dollar sp--special-self-insert-commands))
  (setq sp--special-self-insert-commands (delete `TeX-insert-quote sp--special-self-insert-commands))
  ;; After selecting a region, we can wrap it in parenthesis or quotes.
  (setq sp-autowrap-region t))

;; Custom folding of some macros and commands I normally use
(after! tex-fold
  ;; Custom macro transformation functions
  (defun +boy--latex-fold-autoref (arg)
    "Extract text from ARG up until the first `:', capitalize, format and return it."
    (let ((word (car (split-string arg ":" t))))
      (if word
          (concat (capitalize word) ". XX")
        "Ref. XX")))
  ;; Set custom folds for Macros
  (add-to-list 'TeX-fold-macro-spec-list '("{1}" ("ac" "acf" "Ac" "Acf" "title")))
  (add-to-list 'TeX-fold-macro-spec-list '("{1}s" ("acp" "acfp" "Acp" "Acfp")))
  (add-to-list 'TeX-fold-macro-spec-list '("[authors]" ("author")))
  (add-to-list 'TeX-fold-macro-spec-list '("[command {1}]" ("newcommand")))
  (add-to-list 'TeX-fold-macro-spec-list '("[hyphenations]" ("hyphenation")))
  (add-to-list 'TeX-fold-macro-spec-list '("[c]" ("citep" "citet" "parencite")))
  (add-to-list 'TeX-fold-macro-spec-list '("[{1}]" ("citeauthor" "textcite")))
  (add-to-list 'TeX-fold-macro-spec-list '("[hyphenations]" ("hyphenation")))
  (add-to-list 'TeX-fold-macro-spec-list '("[side-note]" ("graffito")))
  (add-to-list 'TeX-fold-macro-spec-list '("[SBOX {1}]" ("sbox")))
  (add-to-list 'TeX-fold-macro-spec-list '(+boy--latex-fold-autoref ("autoref" "Autoref")))
  ;; Custom folds for Environments
  (add-to-list 'TeX-fold-env-spec-list '("[figure]" ("figure" "figure*")))
  (add-to-list 'TeX-fold-env-spec-list '("[table]" ("table")))
  (add-to-list 'TeX-fold-env-spec-list '("[acronyms]" ("acronym"))))

;; Custom fontifications for commands that might appear in my LaTeX files
(after! latex
  (add-to-list 'font-latex-match-reference-keywords '("captionof" "{[{"))
  (add-to-list 'font-latex-match-reference-keywords '("graffito" "{"))
  (add-to-list 'font-latex-match-reference-keywords '("autoref" "[{"))
  (add-to-list 'font-latex-match-reference-keywords '("Autoref" "[{"))
  (add-to-list 'font-latex-match-reference-keywords '("footref" "{")))

(after! latex
  ;; Macro to replace $$ with \(\)
  (fset '+boy/replace-tex-math-sym
   (lambda (&optional arg)
     "Macro to replace $$ with \(\)."
     (interactive "p")
     (kmacro-exec-ring-item
      (quote ([134217848 105 115 101 97 114 99 104 32 102 111 114 return
               36 return 2 4 92 40 67108896 6 6 23 134217848 105 115 101 return
               36 return backspace 25] 0 "%d")) arg))))

;; Load the org table package
(use-package! org-table ;; internal package
  :commands (orgtbl-mode)
  :init
  ;(add-hook! LaTeX-mode #'orgtbl-mode)
  :config
  (when (modulep! :feature snippets)
    (require 'yasnippet)
    (map! :map orgtbl-mode-map
          "C-c o s"  #'+boy/print-table-send-cmd
          "C-c o r"  #'+boy/print-table-rcv-cmd)))

;; TextLint support in flycheck for LaTeX
(when (modulep! :tools flycheck)
  (after! flycheck
    (setq flycheck-textlint-plugin-alist
          '((latex-mode . "textlint-plugin-latex")))))
