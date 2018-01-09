;;; modules/private/boy/+latex.el -*- lexical-binding: t; -*-

(set! :latex-bibtex-file "/home/boy/Documents/CASED/Papers/bib/TK.bib")
(set! :latex-bibtex-dir "/home/boy/Documents/CASED/Papers")

;; Change some fonts from the doom-one theme for the sections in LaTeX.
(add-hook! LaTeX-mode
  (custom-theme-set-faces 'doom-one
  '(font-latex-sectioning-1-face ((t (:background "#23272e" :foreground "#a9a1e1" :weight bold
                                      :bold bold :height 1.6))))
  '(font-latex-sectioning-2-face ((t (:inherit 'font-latex-sectioning-1-face
                                      :foreground "#51afef" :height 0.9))))
  '(font-latex-sectioning-3-face ((t (:inherit 'font-latex-sectioning-2-face
                                      :foreground "#a9a1e1" :height 0.9))))
  '(font-latex-sectioning-4-face ((t (:inherit 'font-latex-sectioning-3-face
                                      :foreground "violet" :height 0.9))))
  '(font-latex-sectioning-5-face ((t (:inherit 'font-latex-sectioning-4-face
                                      :foreground "white" :height 1.0))))))

;; Custom folding of some macros and commands I normally use.
(after! tex-fold
  ;; Set custom folds for Macros
  (add-to-list 'TeX-fold-macro-spec-list '("{1}" ("ac" "acf" "title")))
  (add-to-list 'TeX-fold-macro-spec-list '("{1}s" ("acp" "acfp")))
  (add-to-list 'TeX-fold-macro-spec-list '("[authors]" ("author")))
  (add-to-list 'TeX-fold-macro-spec-list '("[command {1}]" ("newcommand")))
  (add-to-list 'TeX-fold-macro-spec-list '("[hyphenations]" ("hyphenation")))
  ;; Custom folds for Environments
  (add-to-list 'TeX-fold-env-spec-list '("[figure]" ("figure")))
  (add-to-list 'TeX-fold-env-spec-list '("[table]" ("table")))
  (add-to-list 'TeX-fold-env-spec-list '("[acronyms]" ("acronym"))))

