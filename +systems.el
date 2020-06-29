;;; ~/.doom.d/+systems.el -*- lexical-binding: t; -*-

(when (string= "DE-L081832" (system-name))
  (after! ivy
    ;; remove the path separator that would make rg from chocolatey
    ;; ... exit with an error
    (setq counsel-rg-base-command
          (seq-difference counsel-rg-base-command
                          '(" --path-separator=//"
                            " --path-separator //"
                            "--path-separator"
                            "//"))))
  ;; org directory of work env
  (after! org
    (setq org-directory "~/Documents/work/org/"
          +bibliography-notes-dir "~/Documents/work/org/bib/")))
