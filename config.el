;;; config.el --- The Configuration of the Boy

(defvar boy--synonyms-key ""
  "API key from http://thesaurus.altervista.org that gives us synonyms.")

;; Private things others should not see ;D
(load! "+private" nil t) ; do not complain if file does not exist
;; Load personalized bindings
(load! "+bindings")
;; Personalized functions
(load! "+functions")
;; Anything that modifies the way popups spawn
(load! "+popups")
;; Theme related things
(load! "+themes")

;; Configuration of DOOM lang
(load! "+latex")
(load! "+org")
;; Configuration of DOOM tools
(load! "+tools")
;; Configuration of DOOM ui
(load! "+ui")
;; Config of DOOM feature
(load! "+feature")
;; Config of DOOM completion
(load! "+completion")
;; Config of DOOM editor
(load! "+editor")
;; Config of DOOM term
(load! "+term")

;; Smooth mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))  ; scroll two lines at a time
      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
      mouse-wheel-follow-mouse t                    ; scroll window under mouse
      scroll-step 1)

;; Backups
(setq make-backup-files t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Visual fill column mode by default
;; (setq visual-fill-column-width 120)
;; (add-hook! '(text-mode-hook prog-mode-hook) #'visual-fill-column-mode)

;; Window splitting logic
(setq window-combination-resize t ; after splitting, rebalance windows
      window-combination-limit nil)
;; Custom split: always vertical except if average window column < 80
;(setq split-window-preferred-function #'+boy/split-window-sensibly)

;; Scrolling commands do not cancel isearch
(setq isearch-allow-scroll t)

;; Don't save undo-tree history
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

(after! ibuffer
  ;; nearly all of this is the default layout
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 50 50 :left :elide) ; changed
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))

;; Pop mark improvements (http://endlessparentheses.com/faster-pop-to-mark-command.html)
;; pop mark as in C-u C-SPC C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)
;; Pop until marker actually moves
(advice-add 'pop-to-mark-command :around #'+boy/multi-pop-to-mark)

;; Enable narrow to region
(put 'narrow-to-region 'disabled nil)

;; ---------------------------------------------------------------------------------
;; Additional Packages
;; ---------------------------------------------------------------------------------

;; Show me where I made the last change in a document.
(def-package! goto-last-change
  :commands goto-last-change)

;; Resize windows interactively.
(def-package! resize-window
  :commands (resize-window))

;; Latex synonyms
(def-package! www-synonyms
  :if (not (or (null boy--synonyms-key) (string= "" boy--synonyms-key)))
  :commands (www-synonyms-insert-synonym www-synonyms-change-language)
  :config
  (setq www-synonyms-key boy--synonyms-key))

(def-package! ag
  :defer t
  :init
  (setq ag-highlight-search t
        ag-reuse-buffers t))
