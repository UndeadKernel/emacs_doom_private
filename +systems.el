;;; +systems.el -*- lexical-binding: t; -*-

(when (string= "go-vm99d8264d" (system-name))
  ;; Set default dict to GB
  (after! flyspell
    (setopt ispell-dictionary "en_GB"))
  ;; Enable org-hermes-mode when opening an org file
  (when (modulep! :lang org-hermes)
    (add-hook! org-mode #'org-hermes-mode))
  ;; Custom org configuration
  (after! org
    (setq org-directory "~/documents/org/"
        org-agenda-files (list org-directory)
        org-archive-location (concat org-directory ".archive/%s::")
        org-roam-directory (concat org-directory "roam/")
        +bibliography-notes-dir (concat org-directory "bib/"))))

;; Office 3 Laptop
(when (string= "LT05133" (system-name))
  (after! org
    (org-link-set-parameters "explorer"
                             :follow #'+boy/run-on-explorer)
    (add-to-list 'org-file-apps '("\\.docx?\\'" . "explorer.exe %s"))
    (add-to-list 'org-file-apps '("\\.xlsx?\\'" . "explorer.exe %s"))
  (after! org-download
    (setopt org-download-screenshot-method
            "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\""))))

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
