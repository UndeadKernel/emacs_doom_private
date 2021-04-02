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
;; Different config for different systems
(load! "+systems")
;; Patches of functions to fix other packages
(load! "+patches")

;; Configuration of DOOM lang
(load! "+latex")
(load! "+org")
(load! "+lang")
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
(setq mouse-wheel-scroll-amount '(1) ; scroll two lines at a time
      mouse-wheel-progressive-speed t ; don't accelerate scrolling
      mouse-wheel-follow-mouse t) ; scroll window under mouse

;; Backups
(setq make-backup-files t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Favor visual line wrapping
(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; Visual fill column mode by default
;;(setq visual-fill-column-width 120)
;;(add-hook! '(text-mode-hook prog-mode-hook) #'visual-fill-column-mode)

;; Window splitting logic
(setq window-combination-resize t ; after splitting, rebalance windows
      window-combination-limit nil)
;; Custom split: always vertical except if average window column < 80
;(setq split-window-preferred-function #'+boy/split-window-sensibly)

;; Scrolling commands do not cancel isearch
(setq isearch-allow-scroll t)

;; Don't save undo history
(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)
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
(use-package! goto-last-change
  :commands goto-last-change)

;; Resize windows interactively.
(use-package! resize-window
  :commands (resize-window))

;; Latex synonyms
(use-package! www-synonyms
  :if (not (or (null boy--synonyms-key) (string= "" boy--synonyms-key)))
  :commands (www-synonyms-insert-synonym www-synonyms-change-language)
  :config
  (setq www-synonyms-key boy--synonyms-key))

(use-package! jupyter
  ;; :load-path ("~/src/emacs-jupyter" "~/src/emacs-zmq")
  :after org
  :init
  (setq jupyter-eval-use-overlays t))


(use-package! pacfiles-mode
  :commands (pacfiles pacfiles-start)
  :config
  (set-popup-rule! "^\\*pacfiles.*" :ignore t))

(setq +bibliography-notes-dir "~/documents/org/")

(use-package! command-log-mode
  :commands global-command-log-mode
  :init
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode t
        command-log-mode-window-font-size 0
        command-log-mode-window-size 80)
  :config
  (setq clm/log-command-exceptions*
        (append clm/log-command-exceptions*
                '(+boy/up-scroll
                  +boy/down-scroll))))
