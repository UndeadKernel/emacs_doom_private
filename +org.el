;;; private/boy/+org.el -*- lexical-binding: t; -*-

(setq +org-dir "~/Documents/org/")

;; ORG config
(after! org
  (setq org-startup-folded nil ; do not start folded
        org-tags-column -100 ; the column to the right to align tags
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
                 :prepend t)) ; properties
  (add-to-list 'org-capture-templates
               '("hp" "Thesis Check-in Pages"
                 table-line  ; type
                 (file "thesis.org") ; target
                 "|%U|XXXXXXXX||%^{pages}|%^{comment}|" ; template
                 :prepend t)) ; properties
  (add-to-list 'org-capture-templates
               '("ht" "Thesis TODO"
                 entry  ; type
                 (file+headline "thesis.org" "TODOs") ; target
                 "* [ ] %?\n%i" ; template
                 :prepend t :kill-buffer t)) ; properties
  (add-to-list 'org-capture-templates
               '("hT" "Thesis TODO w/link"
                 entry  ; type
                 (file+headline "thesis.org" "TODOs") ; target
                 "* [ ] %?\nLINK: %l\n%i" ; template
                 :prepend t :kill-buffer t)) ; properties
  (add-to-list 'org-capture-templates
               '("hf" "Thesis FIXME"
                 entry  ; type
                 (file+headline "thesis.org" "FIXMEs") ; target
                 "* [ ] %?\n%i" ; template
                 :prepend t :kill-buffer t)) ; properties
  (add-to-list 'org-capture-templates
               '("hF" "Thesis FIXME w/link"
                 entry  ; type
                 (file+headline "thesis.org" "FIXMEs") ; target
                 "* [ ] %?\nLINK: %l\n%i" ; template
                 :prepend t :kill-buffer t)) ; properties
  (add-to-list 'org-capture-templates
               '("hn" "Thesis Note"
                 entry  ; type
                 (file+headline "thesis.org" "NOTEs") ; target
                 "* %u %?\n%i" ; template
                 :prepend t :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("hN" "Thesis Note w/link"
                 entry  ; type
                 (file+headline "thesis.org" "NOTEs") ; target
                 "* %u %?\nLINK: %l\n%i" ; template
                 :prepend t :kill-buffer t)))

(after! ob-ipython
  ;; ob-ipython focuses the output window, instead, leave focus where it was.
  (advice-add 'ob-ipython--output :override #'+boy*ob-ipython--output))
