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
          org-agenda-files (list org-directory)
          org-archive-location (concat org-directory ".archive/%s::")
          +bibliography-notes-dir "~/Documents/work/org/bib/")
    (add-to-list 'org-file-apps '("\\.docx?\\'" . system))))

(when (string= "SPC-VM-C-CG" (system-name))
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
          org-agenda-files (list org-directory)
          org-archive-location (concat org-directory ".archive/%s::")
          +bibliography-notes-dir "~/Documents/work/org/bib/")
    (add-to-list 'org-file-apps '("\\.docx?\\'" . system)))

  ;; use putty's plink as default tramp method
  (setq tramp-default-method "plink")
  ;; set a custom path to find git-gui--askpass
  (setenv "SSH_ASKPASS"
          "C:\\Program Files\\Git\\mingw64\\libexec\\git-core\\git-gui--askpass"))
