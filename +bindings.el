;;; private/boy/+bindings.el -*- lexical-binding: t; -*-

(map!
 "M-x" 'execute-extended-command
 "C-x C-b" 'ibuffer-list-buffers
 ;; My function bindings (requires my autoloads files)
 "M-n"           '+boy/down-scroll
 "M-p"           '+boy/up-scroll
 "M-d"           '+boy/delete-word
 "<M-backspace>" '+boy/backward-delete-word
 "C-k"           '+boy/delete-line
 "C-M-q"         '+boy/unfill-paragraph
 "S-<f1>"        '+boy/macro-on
 "<f1>"          'call-last-kbd-macro
 ;; Editor related bindings
  "C-a" #'doom/backward-to-bol-or-indent
 ;"C-e" #'doom/forward-to-last-non-comment-or-eol
 [remap newline] #'newline-and-indent
 "C-S-s" 'swiper
 ;; Buffer related bindings
 "C-x b" 'persp-switch-to-buffer
 "C-x B" 'switch-to-buffer
 "C-x k" 'doom/kill-this-buffer-in-all-windows
 "C-S-<left>"  '+boy/window-move-left
 "C-S-<right>" '+boy/window-move-right
 "C-S-<up>"    '+boy/window-move-up
 "C-S-<down>"  '+boy/window-move-down
 ;; Switching windows
 "C-x p"   '+popup/other
 "C-x C-o" '+boy/switch-to-last-window
 "C-x O"   'switch-window-then-swap-buffer
 ;; Doom emacs bindings
 "C-c C-s" 'doom/open-scratch-buffer
 "C-`"     '+popup/toggle
 "C-~"     '+popup/raise
 ;; Misc plugins
 "<f9>" '+neotree/open
 "C-=" 'er/expand-region
 "C-c ." 'goto-last-change ; requires private package 'goto-last-change'
 "C-'" 'imenu-list-smart-toggle
 ;; Org capture
 "C-c c" 'org-capture
 "C-c C" (λ! (require 'org-capture) (call-interactively 'org-capture-goto-target))
 ;; Smart-forward
 "M-<up>" 'smart-up
 "M-<down>" 'smart-down
 "M-<left>" 'smart-backward
 "M-<right>" 'smart-forward
 ;; smartparens
 "C-M-a" 'sp-beginning-of-sexp
 "C-M-e" 'sp-end-of-sexp
 "C-M-f" 'sp-forward-sexp
 "C-M-b" 'sp-backward-sexp
 "C-M-d" 'sp-splice-sexp
 ;; Company mode
 "<C-tab>" '+company/complete
 ;; Counsel Bindings
 "C-h b" 'counsel-descbinds
 ;; Repl Toggle
 "C-c C-z" '+eval/open-repl
;; Magit/git bindings
 (:prefix "C-c m"
   "s" 'magit-status
   "i" '+vcs/git-browse-issues
   "b" '+vcs/git-browse)
 ;; Bury popup buffers with only C-g
 ;; (:map +popup-buffer-mode-map
 ;;   "C-g" '+popup/close)
 ;; Working with windows, workgroups and stuff.
 ;;"<pause>" (λ! (doom/workgroup-load (concat wg-workgroup-directory doom-wg-perpetual)))
 (:prefix "C-c w"
   "d" '+workspace/display
   "r" '+workspace/rename
   "c" '+workspace/new
   "k" '+workspace/delete
   "s" '+workspace/save-session
   "l" '+workspace/load-session
   "o" 'doom/kill-other-buffers
   "u" 'winner-undo
   "U" 'winner-redo
   "p" '+workspace/switch-left
   "n" '+workspace/switch-right
   "h" 'resize-window ; requires private package 'resize-window'
   "1" (λ! (+workspace/switch-to 0))
   "2" (λ! (+workspace/switch-to 1))
   "3" (λ! (+workspace/switch-to 2))
   "4" (λ! (+workspace/switch-to 3))
   "5" (λ! (+workspace/switch-to 4))
   "6" (λ! (+workspace/switch-to 5))
   "7" (λ! (+workspace/switch-to 6))
   "8" (λ! (+workspace/switch-to 7))
   "9" (λ! (+workspace/switch-to 8))
   "0" '+workspace/switch-to-last)
 ;; Restore common editing keys (and ESC) in minibuffer
 (:map (minibuffer-local-map
        minibuffer-local-ns-map
        minibuffer-local-completion-map
        minibuffer-local-must-match-map
        minibuffer-local-isearch-map
        read-expression-map)
   "C-g" #'abort-recursive-edit
   "C-a" #'move-beginning-of-line
   "M-z" #'doom/minibuffer-undo)
 ;; Company mode and the like
 (:after company
   (:map company-active-map
     "C-o"        'company-search-kill-others
     "C-n"        'company-select-next
     "C-p"        'company-select-previous
     "C-h"        'company-quickhelp-manual-begin
     "C-S-h"      'company-show-doc-buffer
     "C-S-s"      'company-search-candidates
     "C-s"        'company-filter-candidates
     "<C-tab>"    'company-complete-common-or-cycle
     [tab]        'company-complete-common-or-cycle
     [backtab]    'company-select-previous
     "C-g"        (λ! (company-abort))
     [C-return]   'counsel-company)
 (:map company-search-map
   "C-n"        'company-search-repeat-forward
   "C-p"        'company-search-repeat-backward
   "C-s"        (λ! (company-search-abort) (company-filter-candidates))
   "C-g"        'company-search-abort))
 ;; NeoTree bindings
 (:after neotree
   :map neotree-mode-map
   "q"       'neotree-hide
   [return]  'neotree-enter
   "RET"     'neotree-enter
   "SPC"     'neotree-quick-look
   "v"       'neotree-enter-vertical-split
   "s"       'neotree-enter-horizontal-split
   "c"       'neotree-create-node
   "D"       'neotree-delete-node
   "g"       'neotree-refresh
   "r"       'neotree-rename-node
   "R"       'neotree-refresh
   "h"       '+neotree/collapse-or-up
   "l"       '+neotree/expand-or-open
   "n"       'neotree-next-line
   "p"       'neotree-previous-line
   "N"       'neotree-select-next-sibling-node
   "P"       'neotree-select-previous-sibling-node)
 ;; Refactoring and compilation
 (:map prog-mode-map
   "M-RET" 'emr-show-refactor-menu)
 (:after cc-mode
   (:map c++-mode-map
     "M-RET" 'srefactor-refactor-at-point)
   (:map c-mode-map
     "M-RET" 'srefactor-refactor-at-point))
 (:after help-mode
   ;; (:map help-map
   ;;   "e" 'doom/popup-toggle-messages)
   (:map help-mode-map
     "o" 'ace-link-help
     ">" 'help-go-forward
     "<" 'help-go-back))
 (:after info
   (:map Info-mode-map
     "o" 'ace-link-info))
 ;; Yasnippet
 (:after yasnippet
   ;; keymap while yasnippet is active
   (:map yas-minor-mode-map
     "C-c TAB" 'doom/yas-expand-or-insert)
   ;; keymap while editing an inserted snippet
   (:map yas-keymap
     "C-e"           'snippets/goto-end-of-field
     "C-a"           'snippets/goto-start-of-field
     "<S-tab>"       'yas-prev-field
     "<M-backspace>" '+snippets/delete-to-start-of-field
     [backspace]     '+snippets/delete-backward-char
     [delete]      '+snippets/delete-forward-char-or-field))
 ;; Flycheck
 (:after flycheck
   (:map flycheck-error-list-mode-map
     "C-n" 'flycheck-error-list-next-error
     "C-p" 'flycheck-error-list-previous-error
     "RET" 'flycheck-error-list-goto-error))
 ;; ivy stuff
 (:after ivy
   (:map ivy-minibuffer-map
     "C-g" 'keyboard-escape-quit))
 ;; magit stuff
 (:after magit
   (:map magit-mode-map
     ;; Don't let Tab binding in my bindings conflict with Tab in magit
     "<tab>" 'magit-section-toggle))
 (:after latex
   (:when (not (or (null boy--synonyms-key) (string= "" boy--synonyms-key)))
     ("C-c s" 'www-synonyms-insert-synonym)))
 ;; (:after ein-notebooklist
 ;;   (:map ein:notebooklist-mode-map
 ;;     "o" 'doom/ace-link-ein))
 )

