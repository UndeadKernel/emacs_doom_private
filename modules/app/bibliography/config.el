;;; app/bibliography/config.el -*- lexical-binding: t; -*-

(defvar +bibliography-notes-file "bibliography.org"
  "File name of the main bibliography file.")

(defvar +bibliography-bib-file "bibliography.bib"
  "File name of the .bib file created with org tangle out of all bibtex entries.")

(defvar +bibliography-notes-dir org-directory
  "Directory where `+bibliography-notes-file' is located.")

(defvar +bibliography-pdfs-dir (concat +bibliography-notes-dir "pdfs/")
  "Location where PDFs are searched for.")

(defvar +bibliography-workspace-name "bibliography"
  "Name used for the workspace of this app.")

(defvar +bibliography--wconf nil
  "Hold the window configuration before `+bibliography/init' is invoked
(when workspaces are not used).")

(defvar +bibliography--buffer nil
  "The buffer where the bibliography is shown.")
