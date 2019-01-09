;;; private/boy/+bindings.el -*- lexical-binding: t; -*-

;; Change the default key of persp-mode to avoid conflicts with projectile.
(setq persp-keymap-prefix (kbd "C-c e"))
(after! projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(map!
 "M-x"           #'execute-extended-command
 "C-x C-b"       #'ibuffer-list-buffers
 "M-n"           #'+boy/down-scroll
 "M-p"           #'+boy/up-scroll
 "M-d"           #'+boy/delete-word
 "<M-backspace>" #'+boy/backward-delete-word
 "C-k"           #'+boy/kill-line
 "C-M-q"         #'+boy/unfill-paragraph
 "S-<f1>"        #'+boy/macro-on
 "<f1>"          #'call-last-kbd-macro
 ;; Editor related bindings
 "C-a"           #'doom/backward-to-bol-or-indent
 [remap newline] #'newline-and-indent
 "C-j"           #'+default/newline
 "C-S-s"         #'swiper
 ;; Buffer related bindings
 "C-x b"       #'persp-switch-to-buffer
 "C-x B"       #'switch-to-buffer
 "C-x k"       #'doom/kill-this-buffer-in-all-windows
 "s-<left>"    #'+boy/window-move-left
 "s-<right>"   #'+boy/window-move-right
 "s-<up>"      #'+boy/window-move-up
 "s-<down>"    #'+boy/window-move-down
 "C-s-<left>"  #'+boy/window-move-far-left
 "C-s-<right>" #'+boy/window-move-far-right
 "C-s-<up>"    #'+boy/window-move-very-top
 "C-s-<down>"  #'+boy/window-move-very-bottom
 ;; Switching windows
 "C-x p"   #'+popup/other
 "C-x C-o" #'+boy/switch-to-last-window
 "C-x O"   #'switch-window-then-swap-buffer
  ;; Restore common editing keys in minibuffer
 (:map (minibuffer-local-map
        minibuffer-local-ns-map
        minibuffer-local-completion-map
        minibuffer-local-must-match-map
        minibuffer-local-isearch-map
        read-expression-map)
   "C-g" #'abort-recursive-edit
   "C-a" #'move-beginning-of-line)
 ;; Doom emacs bindings
 (:prefix "C-c d"
   :desc "Dashboard"                 "d" #'+doom-dashboard/open
   :desc "Recent files"              "f" #'recentf-open-files
   (:when (featurep! :ui neotree)
   :desc "Open neotree"              "n" #'+neotree/open
     :desc "File in neotree"         "N" #'neotree/find-this-file)
   (:when (featurep! :ui treemacs)
     :desc "Toggle treemacs"         "n" #'+treemacs/toggle
     :desc "File in treemacs"        "N" #'+treemacs/find-file)
   :desc "Popup other"               "o" #'+popup/other
   :desc "Popup toggle"              "t" #'+popup/toggle
   :desc "Popup close"               "c" #'+popup/close
   :desc "Popup close all"           "C" #'+popup/close-all
   :desc "Popup raise"               "r" #'+popup/raise
   :desc "Popup restore"             "R" #'+popup/restore
   :desc "Scratch buffer"            "s" #'doom/open-scratch-buffer
   :desc "Switch to scratch buffer"  "S" #'doom/switch-to-scratch-buffer
   :desc "Sudo this file"            "u" #'doom/sudo-this-file
   :desc "Eshell popup"              "e" #'+eshell/open-popup
   :desc "Eshell open"               "E" #'+eshell/open
   :desc "Reload Private Config"     "R" #'doom/reload)
 :desc "Popup toggle"                "C-`" #'+popup/toggle
 ;; Org related bindings
 "C-c o" nil
 (:prefix "C-c o"
   :desc "Do what I mean"          "o"     #'+org/dwim-at-point
   :desc "Sync org caldav"         "s"     #'org-caldav-sync
   :desc "Agenda"                  "a a"   #'org-agenda
   :desc "Todo list"               "a t"   #'org-todo-list
   :desc "Tags view"               "a m"   #'org-tags-view
   :desc "View search"             "a v"   #'org-search-view
   :desc "Capture"                 "c"     #'org-capture
   :desc "Goto capture"            "C"     (λ! (require 'org-capture) (call-interactively #'org-capture-goto-target))
   :desc "Switch org buffers"      "b"     #'org-switchb
   :desc "Export beamer to latex"  "e l b" #'org-beamer-export-to-latex
   :desc "Export beamer as latex"  "e l B" #'org-beamer-export-as-latex
   :desc "Export beamer as pdf"    "e l P" #'org-beamer-export-to-pdf
   :desc "Link store"              "l"     #'org-store-link
   :desc "Org hydra"               "h"     #'+boy/org-babel-hydra/body)
 ;; Snippets
 (:prefix "C-c s"
   :desc "New snippet"           "n" #'yas-new-snippet
   :desc "Insert snippet"        "i" #'yas-insert-snippet
   :desc "Find snippet"          "s" #'+default/find-in-snippets
   :desc "Find snippet for mode" "S" #'+default/browse-snippets
   :desc "Find global snippet"   "/" #'yas-visit-snippet-file
   :desc "Reload snippets"       "r" #'yas-reload-all
   :desc "Create Temp Template"  "c" #'aya-create
   :desc "Use Temp Template"     "e" #'aya-expand)
 ;; Version control bindings
 (:prefix "C-c v"
   :desc "Browse issues tracker" "i" #'+vc/git-browse-issues
   :desc "Browse remote"         "o" #'+vc/git-browse
   :desc "Diff current file"     "d" #'magit-diff-buffer-file
   :desc "Git revert hunk"       "r" #'git-gutter:revert-hunk
   :desc "Git stage file"        "S" #'magit-stage-file
   :desc "Git stage hunk"        "s" #'git-gutter:stage-hunk
   :desc "Git time machine"      "t" #'git-timemachine-toggle
   :desc "Git unstage file"      "U" #'magit-unstage-file
   :desc "Initialize repo"       "I" #'magit-init
   :desc "List repositories"     "L" #'magit-list-repositories
   :desc "Magit blame"           "b" #'magit-blame-addition
   :desc "Magit buffer log"      "l" #'magit-log-buffer-file
   :desc "Magit commit"          "c" #'magit-commit-create
   :desc "Magit status"          "g" #'magit-status
   :desc "Next hunk"             "]" #'git-gutter:next-hunk
   :desc "Previous hunk"         "[" #'git-gutter:previous-hunk)
 ;; Working with windows, workgroups and stuff.
 (:prefix "C-c w"
   :desc "Display workspaces"           "d" #'+workspace/display
   :desc "Rename workspace"             "r" #'+workspace/rename
   :desc "Create workspace"             "c" #'+workspace/new
   :desc "Delete workspace"             "k" #'+workspace/delete
   :desc "Save session"                 "s" (λ! (let ((current-prefix-arg '(4))) (call-interactively #'+workspace/save-session)))
   :desc "Save workspace"               "S" #'+workspace/save
   :desc "Load session"                 "l" #'+workspace/load-session
   :desc "Load last autosaved session"  "L" #'+workspace/load-last-session
   :desc "Kill other buffers"           "o" #'doom/kill-other-buffers
   :desc "Undo window config"           "u" #'winner-undo
   :desc "Redo window config"           "U" #'winner-redo
   :desc "Switch to left workspace"     "p" #'+workspace/switch-left
   :desc "Switch to right workspace"    "n" #'+workspace/switch-right
   :desc "Resize window"                "h" #'resize-window ; requires private package 'resize-window'
   :desc "Switch to workspace 1"        "1" (λ! (+workspace/switch-to 0))
   :desc "Switch to workspace 2"        "2" (λ! (+workspace/switch-to 1))
   :desc "Switch to workspace 3"        "3" (λ! (+workspace/switch-to 2))
   :desc "Switch to workspace 4"        "4" (λ! (+workspace/switch-to 3))
   :desc "Switch to workspace 5"        "5" (λ! (+workspace/switch-to 4))
   :desc "Switch to workspace 6"        "6" (λ! (+workspace/switch-to 5))
   :desc "Switch to workspace 7"        "7" (λ! (+workspace/switch-to 6))
   :desc "Switch to workspace 8"        "8" (λ! (+workspace/switch-to 7))
   :desc "Switch to workspace 9"        "9" (λ! (+workspace/switch-to 8))
   :desc "Switch to last workspace"     "0" #'+workspace/switch-to-last)
 ;; Multiple Cursors
 (:when (featurep! :editor multiple-cursors)
   (:prefix "C-c m"
     :desc "Edit lines"         "l"         #'mc/edit-lines
     :desc "Mark next"          "n"         #'mc/mark-next-like-this
     :desc "Unmark next"        "N"         #'mc/unmark-next-like-this
     :desc "Mark previous"      "p"         #'mc/mark-previous-like-this
     :desc "Unmark previous"    "P"         #'mc/unmark-previous-like-this
     :desc "Mark all"           "t"         #'mc/mark-all-like-this
     :desc "Mark all DWIM"      "m"         #'mc/mark-all-like-this-dwim
     :desc "Edit line endings"  "e"         #'mc/edit-ends-of-lines
     :desc "Edit line starts"   "a"         #'mc/edit-beginnings-of-lines
     :desc "Mark tag"           "s"         #'mc/mark-sgml-tag-pair
     :desc "Mark in defun"      "d"         #'mc/mark-all-like-this-in-defun
     :desc "Add cursor w/mouse" "<mouse-1>" #'mc/add-cursor-on-click))

 ;; Plugins

 ;; Misc plugins
 "<f9>"    #'+neotree/open
 "C-="     #'er/expand-region
 "C-c ."   #'goto-last-change ; requires private package 'goto-last-change'
 "C-c p p" #'projectile-switch-project
 ;; Smartparens
 (:after smartparens
   (:map smartparens-mode-map
     "C-M-a"     #'sp-beginning-of-sexp
     "C-M-e"     #'sp-end-of-sexp
     "C-M-f"     #'sp-forward-sexp
     "C-M-b"     #'sp-backward-sexp
     "C-M-d"     #'sp-splice-sexp
     "C-M-k"     #'sp-kill-sexp
     "C-M-t"     #'sp-transpose-sexp
     "C-<right>" #'sp-forward-slurp-sexp
     "M-<right>" #'sp-forward-barf-sexp
     "C-<left>"  #'sp-backward-slurp-sexp
     "M-<left>"  #'sp-backward-barf-sexp))
 ;; Company mode
 "C-;" #'+company/complete
 ;; Counsel
 (:when (featurep! :completion ivy)
   (:after counsel
     (:map counsel-ag-map
       [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
       "C-SPC"    #'ivy-call-and-recenter ; preview
       "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action))))
 "C-h b" #'counsel-descbinds
 "C-M-y" #'counsel-yank-pop
 "C-h F" #'counsel-faces
 "C-h p" #'counsel-package
 "C-h a" #'counsel-apropos
 "C-h V" #'counsel-set-variable
 "C-'"   #'counsel-imenu
 ;; Repl Toggle
 "C-c C-z" #'+eval/open-repl
 ;; Company mode and the like
 (:after company
   (:map company-active-map
     "C-o"      #'company-search-kill-others
     "C-n"      #'company-select-next
     "C-p"      #'company-select-previous
     "C-h"      #'company-show-doc-buffer
     "C-s"      #'company-search-candidates
     "M-s"      #'company-filter-candidates
     "C-;"      #'company-complete-common-or-cycle
     "TAB"      #'company-complete-common-or-cycle
     [backtab]  #'company-select-previous
     "C-RET"    #'counsel-company)
   (:map company-search-map
     "C-n"        #'company-search-repeat-forward
     "C-p"        #'company-search-repeat-backward
     "C-s"        (λ! (company-search-abort) (company-filter-candidates))))
 ;; NeoTree bindings
 (:after neotree
   :map neotree-mode-map
   "q"       #'neotree-hide
   [return]  #'neotree-enter
   "RET"     #'neotree-enter
   "SPC"     #'neotree-quick-look
   "v"       #'neotree-enter-vertical-split
   "s"       #'neotree-enter-horizontal-split
   "c"       #'neotree-create-node
   "D"       #'neotree-delete-node
   "g"       #'neotree-refresh
   "r"       #'neotree-rename-node
   "R"       #'neotree-refresh
   "h"       #'+neotree/collapse-or-up
   "l"       #'+neotree/expand-or-open
   "n"       #'neotree-next-line
   "p"       #'neotree-previous-line
   "N"       #'neotree-select-next-sibling-node
   "P"       #'neotree-select-previous-sibling-node)
 ;; Refactoring and compilation
 (:map prog-mode-map
   "M-RET" #'emr-show-refactor-menu)
 (:after cc-mode
   (:map c++-mode-map
     "M-RET" #'srefactor-refactor-at-point)
   (:map c-mode-map
     "M-RET" #'srefactor-refactor-at-point))
 (:after help-mode
   ;; (:map help-map
   ;;   "e" 'doom/popup-toggle-messages)
   (:map help-mode-map
     "o" #'ace-link-help
     ">" #'help-go-forward
     "<" #'help-go-back))
 (:after info
   (:map Info-mode-map
     "o" #'ace-link-info))
 ;; yasnippet
 (:after yasnippet
   ;; keymap while yasnippet is active
   (:map yas-minor-mode-map
     "<C-tab>" #'yas-insert-snippet)
   ;; keymap while editing an inserted snippet
   (:map yas-keymap
     "C-e"           #'+snippets/goto-end-of-field
     "C-a"           #'+snippets/goto-start-of-field
     "<M-right>"     #'+snippets/goto-end-of-field
     "<M-left>"      #'+snippets/goto-start-of-field
     "<M-backspace>" #'+snippets/delete-to-start-of-field
     [backspace]     #'+snippets/delete-backward-char
     [delete]        #'+snippets/delete-forward-char-or-field))
 ;; flycheck
 (:after flycheck
   (:map flycheck-error-list-mode-map
     "C-n" #'flycheck-error-list-next-error
     "C-p" #'flycheck-error-list-previous-error
     "RET" #'flycheck-error-list-goto-error))
 ;; flyspell
 (:after flyspell
   (:map flyspell-mode-map
     "C-;" nil ; Do not override
     "C-M-i" #'flyspell-auto-correct-previous-word))
 ;; ivy
 (:after ivy
   (:map ivy-minibuffer-map
     "TAB" #'ivy-alt-done
     "C-g" #'keyboard-escape-quit))
 ;; magit
 (:after magit
   (:map magit-mode-map
     ;; Don't let Tab binding in my bindings conflict with Tab in magit
     "<tab>" #'magit-section-toggle))
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
 ;; ein notebokks
 (:after ein:notebook-multilang
   (:map ein:notebook-multilang-mode-map
     "C-c h" #'+ein/hydra/body))
 )


(which-key-add-key-based-replacements "C-c !"   "checking")
(which-key-add-key-based-replacements "C-c d p" "doom popups")
(which-key-add-key-based-replacements "C-c d"   "doom")
(which-key-add-key-based-replacements "C-c e"   "perspective")
(which-key-add-key-based-replacements "C-c m"   "mail")
(which-key-add-key-based-replacements "C-c o a" "org agenda")
(which-key-add-key-based-replacements "C-c o e" "org export")
(which-key-add-key-based-replacements "C-c o"   "org")
(which-key-add-key-based-replacements "C-c p"   "projectile")
(which-key-add-key-based-replacements "C-c s"   "snippets")
(which-key-add-key-based-replacements "C-c v"   "versioning")
(which-key-add-key-based-replacements "C-c w"   "workspace")
