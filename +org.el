;;; private/boy/+org.el -*- lexical-binding: t; -*-

(setq +org-dir "~/Documents/org/")

;; ORG config
(after! org
  ;; Face of keyword DONE (Green like strings)
  (set-face-attribute 'org-done nil :foreground "#98be65")
  ; Face of keyword TODO or [ ] (Purple like keywords)
  (set-face-attribute 'org-todo nil :foreground "#c678dd")
  ; Face of ellipsis symbol (Purple like keywords)
  (set-face-attribute 'org-ellipsis nil :foreground "#c678dd")
  ; Face of the entire headline of a DONE line
  (set-face-attribute 'org-headline-done nil :foreground nil)
  (setq org-tags-column -100 ; the column to the right to align tags
        org-log-done 'time ; record the time when an element was marked done/checked
        org-fontify-done-headline nil ; do not change the font of DONE items
        org-ellipsis " ↴ ")

  ;; Custom org-capture templates
  (add-to-list 'org-capture-templates
               '("h" "Templates for Thesis related info"))
  (add-to-list 'org-capture-templates
               '("hP" "Thesis Check-out Pages"
                 table-line  ; type
                 (file "thesis.org") ; target
                 "|%U||XXXXXXXX|%^{pages}|%^{comment}|" ; template
                 :prepend t )) ; properties
  (add-to-list 'org-capture-templates
               '("hp" "Thesis Check-in Pages"
                 table-line  ; type
                 (file "thesis.org") ; target
                 "|%U|XXXXXXXX||%^{pages}|%^{comment}|" ; template
                 :prepend t )) ; properties
  (add-to-list 'org-capture-templates
               '("ht" "Thesis TODO"
                 entry  ; type
                 (file+headline "thesis.org" "TODOs") ; target
                 "* [ ] %?\n%i" ; template
                 :prepend t :kill-buffer)) ; properties
  (add-to-list 'org-capture-templates
               '("hT" "Thesis TODO w/link"
                 entry  ; type
                 (file+headline "thesis.org" "TODOs") ; target
                 "* [ ] %?\nLINK: %l\n%i" ; template
                 :prepend t :kill-buffer)) ; properties
  (add-to-list 'org-capture-templates
               '("hf" "Thesis FIXME"
                 entry  ; type
                 (file+headline "thesis.org" "FIXMEs") ; target
                 "* [ ] %?\n%i" ; template
                 :prepend t :kill-buffer)) ; properties
  (add-to-list 'org-capture-templates
               '("hF" "Thesis FIXME w/link"
                 entry  ; type
                 (file+headline "thesis.org" "FIXMEs") ; target
                 "* [ ] %?\nLINK: %l\n%i" ; template
                 :prepend t :kill-buffer)) ; properties
  (add-to-list 'org-capture-templates
               '("hn" "Thesis Note"
                 entry  ; type
                 (file+headline "thesis.org" "NOTEs") ; target
                 "* %u %?\n%i" ; template
                 :prepend t :kill-buffer))
  (add-to-list 'org-capture-templates
               '("hN" "Thesis Note w/link"
                 entry  ; type
                 (file+headline "thesis.org" "NOTEs") ; target
                 "* %u %?\nLINK: %l\n%i" ; template
                 :prepend t :kill-buffer)))


