;;; ~/.doom.d/+systems.el -*- lexical-binding: t; -*-

;; Office 3 Laptop
(when (string= "LT05133" (system-name))
  (after! org
    (org-link-set-parameters "explorer"
                             :follow #'+boy/run-on-explorer)
    (add-to-list 'org-file-apps '("\\.docx?\\'" . "explorer.exe %s"))
    (add-to-list 'org-file-apps '("\\.xlsx?\\'" . "explorer.exe %s"))
    (defun +boy/org-roam-find-acronyms ()
      "Get the file path of the `acronyms' node"
      (org-roam-node-file (org-roam-node-from-title-or-alias "acronyms")))
    (defun +boy/org-roam-find-people ()
      "Get the file path of the `people' node"
      (org-roam-node-file (org-roam-node-from-title-or-alias "people")))
    (add-to-list 'org-capture-templates
                 '("r" "Roam Templates"))
    (add-to-list 'org-capture-templates
                 '("ra" "Acronyms"
                   entry  ; type
                   (file +boy/org-roam-find-acronyms) ; target
                   "* %?\n:PROPERTIES:\n:ID: %(format-time-string \"      id-%Y%m%d-%H%M%S\" (current-time) t)\n:END:" ; template
                   :prepend t :kill-buffer t))
    (add-to-list 'org-capture-templates
                 '("rp" "People"
                   entry  ; type
                   (file +boy/org-roam-find-people) ; target
                   "* %?\n:PROPERTIES:\n:ID: %(format-time-string \"      id-%Y%m%d-%H%M%S\" (current-time) t)\n:END:" ; template
                   :prepend t :kill-buffer t))
    ))
  (after! org-download
    (setq! org-download-screenshot-method "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")))

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
