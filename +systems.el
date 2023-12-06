;;; ~/.doom.d/+systems.el -*- lexical-binding: t; -*-

;; Office 2 Laptop
(when (string= "DE-L089725" (system-name))
  ;; org directory of work env
  (setq org-directory "~/work/org/"
        org-agenda-files (list org-directory)
        org-archive-location (concat org-directory ".archive/%s::")
        +bibliography-notes-dir "~/work/org/bib/")
  (after! org
    (add-to-list 'org-file-apps '("\\.docx?\\'" . system))
    (add-to-list 'org-file-apps '("\\.xlsx?\\'" . system)))
  ;; Add a copy function for WSL2
  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "clip.exe")
    (deactivate-mark)))

;; Office 1 Laptop
(when (string= "DE-L081832" (system-name))
  ;; org directory of work env
  (setq org-directory "~/work/org/"
        org-agenda-files (list org-directory)
        org-archive-location (concat org-directory ".archive/%s::")
        +bibliography-notes-dir "~/work/org/bib/")
  (after! org
    (add-to-list 'org-file-apps '("\\.docx?\\'" . system))
    (add-to-list 'org-file-apps '("\\.xlsx?\\'" . system)))
  ;; Add a copy function for WSL2
  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "clip.exe")
    (deactivate-mark)))

;; VirtualBox VM
(when (string= "turtles" (system-name))
  (setq org-directory "~/windows/Documents/work/org/"
        org-agenda-files (list org-directory)
        org-archive-location (concat org-directory ".archive/%s::")
        +bibliography-notes-dir "~/windows/Documents/work/org/bib/"))
