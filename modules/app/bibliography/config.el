;;; app/bibliography/config.el -*- lexical-binding: t; -*-

(defvar +bibliography-notes-file "bibliography.org"
  "File name of the main bibliography file.")

(defvar +bibliography-notes-dir org-directory
  "Directory where `+bibliography-notes-file' is located.")

(defvar +bibliography-pdfs-dir "~/"
  "Location where PDFs are searched for.")

(defvar +bibliography-workspace-name "bibliography"
  "Name used for the workspace of this app.")

;(use-package! zotxt-emacs)

(use-package! org-noter
  :commands (org-noter)
  :config
  (after! pdf-tools
    (setq pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))
  ;;(setq org-noter-notes-mode-map (make-sparse-keymap))
  (require 'org-noter-pdftools))

(use-package! org-pdftools
  :init
  (setq org-pdftools-root-dir +bibliography-pdfs-dir
        org-pdftools-search-string-separator "??")
  :config
  (after! org
    (org-pdftools-setup-link)))

(use-package! org-noter-pdftools
  :after org-noter
  :config
  (after! pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
