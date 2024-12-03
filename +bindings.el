;;; private/boy/+bindings.el -*- lexical-binding: t; -*-

;; Set the "localleader" binding to C-z
(map! "C-z" nil)
(setq doom-localleader-alt-key "C-z")

(map!
 ;; Global Bindings
 "M-n"           #'+boy/down-scroll
 "M-p"           #'+boy/up-scroll
 "M-d"           #'+boy/delete-word
 "<M-backspace>" #'+boy/backward-delete-word
 "<C-backspace>" #'+boy/backward-delete-word
 "C-k"           #'+boy/kill-line
 "C-M-q"         #'+boy/unfill-paragraph
 "S-<f1>"        #'+boy/macro-on
 "<f1>"          #'call-last-kbd-macro
 ;; Buffer related bindings
 "s-<left>"      #'+boy/window-move-left
 "s-<right>"     #'+boy/window-move-right
 "s-<up>"        #'+boy/window-move-up
 "s-<down>"      #'+boy/window-move-down
 "C-s-<left>"    #'+boy/window-move-far-left
 "C-s-<right>"   #'+boy/window-move-far-right
 "C-s-<up>"      #'+boy/window-move-very-top
 "C-s-<down>"    #'+boy/window-move-very-bottom
 ;; Switching windows
 "C-x C-o"       #'+boy/switch-to-last-window
 ;; Switching Workspaces
 "M-="    #'+workspace/display
 "M-1"    #'+workspace/switch-to-0
 "M-2"    #'+workspace/switch-to-1
 "M-3"    #'+workspace/switch-to-2
 "M-4"    #'+workspace/switch-to-3
 "M-5"    #'+workspace/switch-to-4
 "M-6"    #'+workspace/switch-to-5
 "M-7"    #'+workspace/switch-to-6
 "M-8"    #'+workspace/switch-to-7
 "M-9"    #'+workspace/switch-to-8
 "M-0"    #'+workspace/switch-to-final
 (:leader
   (:prefix-map ("f" . "file")
     :desc "New empty buffer"        "n" #'+boy/new-buffer)
   (:prefix-map ("w" . "workspaces/windows")
    :desc "Resize window"            "h" #'resize-window) ; requires private package 'resize-window'
   ;; Org related bindings
   (:prefix-map ("n". "notes")
     :desc "Do what I mean"          "o" #'+org/dwim-at-point
     :desc "Org hydra"               "h" #'+boy/org-babel-hydra/body)
   ;; Toggle bindings
   (:prefix-map ("t" . "toggle")
    :desc "Toggle Command-Log-Mode"  "c" #'global-command-log-mode)
   ;; Unbindings
   (:after eww
     (:map eww-mode-map
       "M-p" nil
       "M-n" nil)))

 ;; Plugins

 ;; misc plugins
 "C-c ."   #'goto-last-change ; requires private package 'goto-last-change'
 ;; undo-fu
 (:when (fboundp #'undo-fu-only-redo)
   "C-?"   #'undo-fu-only-redo)
 ;; objed
 "M-o"     #'objed-activate-object
 "M-["     #'objed-beg-of-object-at-point
 "M-]"     #'objed-end-of-object-at-point
 "C-,"     #'objed-prev-identifier
 "C-."     #'objed-next-identifier
 "C-<"     #'objed-first-identifier
 "C->"     #'objed-last-identifier
 ;; smartparens
 (:after smartparens
   (:map smartparens-mode-map
    "M-(" #'sp-wrap-round
    "C-<right>" #'sp-forward-slurp-sexp
    "C-M-<right>" #'sp-forward-barf-sexp
    "C-<left>" #'sp-backward-slurp-sexp
    "C-M-<left>" #'sp-backward-barf-sexp))
 ;; magit
 (:after magit
   (:map magit-mode-map
     "M-n"     nil ; do not overwrite
     "M-p"     nil
     "M-="     nil
     "M-1"     nil
     "M-2"     nil
     "M-3"     nil
     "M-4"     nil
     "M-5"     nil
     "C-c C-n" #'magit-section-forward-sibling
     "C-c C-p" #'magit-section-backward-sibling))
 ;; pdf-tools
 (:after pdf-tools
   (:map pdf-annot-minor-mode-map
     "a"   #'pdf-annot-add-highlight-markup-annotation
     "s"   #'pdf-annot-add-text-annotation
     "d"   #'pdf-annot-add-underline-markup-annotation
     "f"   #'pdf-annot-add-squiggly-markup-annotation
     "g"   #'pdf-annot-attachment-dired
     "D"   #'pdf-annot-delete)
   (:map (image-mode-map image-minor-mode-map)
    "C-c C-x"  nil))
 ;; org-noter
 (:after org-noter
  (:map org-noter-notes-mode-map
   "M-n"   nil
   "M-p"   nil)
  (:map org-noter-doc-mode-map
   "t"       #'+boy/highlight-and-add-item
   "T"       #'+boy/highlight-and-annot-w-noter
   "C-x C-s" #'+boy/save-pdf-and-notes))

 ;; switch-window
 (:after switch-window
   (:when (modulep! :ui window-select +switch-window)
     "C-x O"         #'switch-window-then-swap-buffer
     "C-x 4 1"       #'switch-window-then-maximize
     "C-x 4 d"       #'switch-window-then-dired
     "C-x 4 f"       #'switch-window-then-find-file
     "C-x 4 o"       #'switch-window-then-display-buffer
     "C-x 4 0"       #'switch-window-then-delete
     "C-x 4 k"       #'switch-window-then-kill-buffer
     (:when (modulep! :ui popup)
       "C-x o"         #'+boy/switch-window
       "C-x p"         (λ! (+boy/switch-window t)))))
 ;; edebug
 (:after edebug
   (:map edebug-mode-map
     "l"   #'recenter-top-bottom))
 ;; org
 (:after org
   (:map org-mode-map
     ;; unset for objed)
     "C-,"   nil))
 ;; flyspell
 (:after flyspell
   (:map flyspell-mode-map
     "C-;"   nil ; Do not override
     "C-,"   nil ; unset for objed
     "C-."   nil ; unset for objed
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
 ;; company mode
 (:map company-active-map
   "C-;"  #'company-complete-selection))

;; eshell
(defun +boy-setup-eshell-bindings ()
  (map!
   (:map eshell-mode-map
     "RET"     #'+boy/eshell-gotoend-or-send
     [return]  #'+boy/eshell-gotoend-or-send)))
(add-hook 'eshell-first-time-mode-hook #'+boy-setup-eshell-bindings)
