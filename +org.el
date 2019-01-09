;;; private/boy/+org.el -*- lexical-binding: t; -*-

(setq +org-dir "~/Documents/org/")

;; ORG config
(after! org
  (setq org-startup-folded nil ; do not start folded
        org-tags-column -100 ; the column to the right to align tags
        org-log-done 'time ; record the time when an element was marked done/checked
        org-fontify-done-headline nil ; do not change the font of DONE items
        org-ellipsis " ↴ "
        org-babel-min-lines-for-block-output 5 ; when to wrap results in #begin_example
        org-return-follows-link t  ; RET follows links
        org-hide-emphasis-markers t ; do not show format markers
        ob-async-no-async-languages-alist '("ipython")  ; do not use async with ob-ipython
        visual-fill-column-width 120) ; size for usage with visual fill column mode

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

;; Enable displaying of inline PDF images in ORG files
;; https://stackoverflow.com/a/35261577/2632102
(add-hook! 'org-mode-hook
  (make-local-variable 'image-type-file-name-regexps)
  (make-local-variable 'image-file-name-extensions)
  (add-to-list 'image-type-file-name-regexps '("\\.pdf\\'" . imagemagick))
  (add-to-list 'image-file-name-extensions "pdf")
  (setq-local imagemagick-types-inhibit (remove 'PDF imagemagick-types-inhibit))
  (setq-local org-image-actual-width nil))

;; Custom hack: Hide source blocks that have the attribute `:hidden'.
(add-hook! 'org-mode-hook (+boy/hide-source-blocks-maybe))

;; Enable visual-fill-column-mode by default
(add-hook! 'org-mode-hook '(visual-fill-column-mode visual-line-mode))
