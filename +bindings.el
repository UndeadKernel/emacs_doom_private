;;; private/boy/+bindings.el -*- lexical-binding: t; -*-

(map!
 "M-n"           #'+boy/down-scroll
 "M-p"           #'+boy/up-scroll
 "M-d"           #'+boy/delete-word
 "<M-backspace>" #'+boy/backward-delete-word
 "<C-backspace>" #'+boy/backward-delete-word
 "C-k"           #'+boy/kill-line
 "C-M-q"         #'+boy/unfill-paragraph
 "S-<f1>"        #'+boy/macro-on
 "<f1>"          #'call-last-kbd-macro
 "C-c p p"       #'projectile-switch-project
 ;; Editor related bindings
 [remap newline] #'newline-and-indent
 "C-j"           #'+default/newline
 ;; Buffer related bindings
 "s-<left>"      #'+boy/window-move-left
 "s-<right>"     #'+boy/window-move-right
 "s-<up>"        #'+boy/window-move-up
 "s-<down>"      #'+boy/window-move-down
 "C-s-<left>"    #'+boy/window-move-far-left
 "C-s-<right>"   #'+boy/window-move-far-right
 "C-s-<up>"      #'+boy/window-move-very-top
 "C-s-<down>"    #'+boy/window-move-very-bottom
 ;; Creating empty buffers
 (:prefix "C-c f"
   :desc "Move this file"   "m" #'doom/move-this-file
   :desc "New empty buffer" "n" #'+boy/new-buffer)
 ;; Switching windows
 "C-x C-o"       #'+boy/switch-to-last-window
 (:prefix "C-c w"
   :desc "Resize window"           "h" #'resize-window) ; requires private package 'resize-window'
 ;; Org related bindings
 (:prefix "C-c o"
   :desc "Do what I mean"          "o" #'+org/dwim-at-point
   :desc "Org hydra"               "h" #'+boy/org-babel-hydra/body
   :desc "Display inline images"   "i" #'org-display-inline-images)
 ;; Snippets
 (:prefix "C-c &"
   :desc "Find snippet"          "s" #'+default/find-in-snippets
   :desc "Find snippet for mode" "S" #'+default/browse-snippets)
 ;; Terminal
 (:prefix "C-c t"
   "t"  #'+eshell/open-popup
   "T"  #'+eshell/open)
 ;; Unbindings
 (:leader
   "`"   nil ; overwrite opening a terminal with this key
   "C-f"  nil) ; unbind projectile find file
 
 ;; Plugins

 ;; Misc plugins
 "C-c ."   #'goto-last-change ; requires private package 'goto-last-change'
 ;; smartparens
 (:after smartparens
   (:map smartparens-mode-map
     "M-(" #'sp-wrap-round))
 ;; magit
 (:after magit
   (:map magit-mode-map
     "M-n"     nil ; do not overwrite
     "M-p"     nil
     "C-c C-n" #'magit-section-forward-sibling
     "C-c C-p" #'magit-section-backward-sibling))
 ;; pdf-tools
 (:after pdf-tools
   (:map pdf-view-mode-map
     "q"   #'pdf-annot-add-highlight-markup-annotation
     "w"   #'pdf-annot-add-text-annotation
     "e"   #'pdf-annot-add-underline-markup-annotation
     "r"   #'pdf-annot-add-squiggly-markup-annotation
     "t"   #'pdf-annot-attachment-dired
     "D"   #'pdf-annot-delete))
 ;; switch-window
 (:after switch-window
   (:when (featurep! :ui window-select +switch-window)
     "C-x O"         #'switch-window-then-swap-buffer
     "C-x 4 1"       #'switch-window-then-maximize
     "C-x 4 d"       #'switch-window-then-dired
     "C-x 4 f"       #'switch-window-then-find-file
     "C-x 4 o"       #'switch-window-then-display-buffer
     "C-x 4 0"       #'switch-window-then-delete
     "C-x 4 k"       #'switch-window-then-kill-buffer
     (:when (featurep! :ui popup)
       "C-x o"         #'+boy/switch-window
       "C-x p"         (lambda () (interactive) (+boy/switch-window t)))))
 ;; edebug
 (:after edebug
   (:map edebug-mode-map
     "l"   #'recenter-top-bottom))
 ;; Refactoring and compilation
 (:map prog-mode-map
   "M-RET" #'emr-show-refactor-menu)
 (:after cc-mode
   (:map c++-mode-map
     "M-RET" #'srefactor-refactor-at-point)
   (:map c-mode-map
     "M-RET" #'srefactor-refactor-at-point))
 ;; flyspell
 (:after flyspell
   (:map flyspell-mode-map
     "C-;"   nil ; Do not override
     "C-M-i" #'flyspell-correct-wrapper
     "M-i"   #'flyspell-auto-correct-previous-word))
 ;; latex
 (:after latex
   (:when (not (or (null boy--synonyms-key) (string= "" boy--synonyms-key)))
     ("C-c y" #'www-synonyms-insert-synonym))
   (:map LaTeX-mode-map
     ;; Do not overwrite my goto-last-change
     "C-c ."   nil
     ;; Replace LaTeX-section with a version that inserts '%' after the section macro
     "C-c C-s" #'+boy/latex-section
     ;; Run LatexMk without asking
     "<f8>"    #'+boy/run-latexmk))
 ;; markdown mode
 (:after markdown-mode
   (:map markdown-mode-map
     "M-b" nil
     "M-n" nil
     "M-p" nil)) ; disable key bindings
 ;; info mode
 (:map Info-mode-map
   "M-n" nil ; disable key bindings
   "M-p" nil)
 )

;; eshell
(defun +boy|setup-eshell-bindings ()
  (map!
   (:map eshell-mode-map
     "M-p" nil
     "M-n" nil
     "C-p" #'eshell-previous-matching-input-from-input
     "C-n" #'eshell-next-matching-input-from-input)))
(add-hook 'eshell-first-time-mode-hook #'+boy|setup-eshell-bindings)
