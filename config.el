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

(use-package! ag
  :defer t
  :init
  (setq ag-highlight-search t
        ag-reuse-buffers t))

(use-package! jupyter
  ;; :load-path ("~/src/emacs-jupyter" "~/src/emacs-zmq")
  :after org
  :init
  ;; use overlays in jupyter-scratch to show evaluation results
  (setq jupyter-eval-use-overlays t)
  :config
  (after! ob-async
    (add-to-list 'ob-async-no-async-languages-alist "jupyter-python"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; (julia . t)
     (python . t)
     (jupyter . t))))


(use-package! pacfiles-mode
  :commands (pacfiles pacfiles-start)
  :config
  (set-popup-rule! "^\\*pacfiles.*" :ignore t))

(use-package! org-trello
  :defer t ;; the package comes with everything needed as an autoload
  :init
  (setq org-trello-files '("~/documents/org/trello.org"))
  ;; HACK: setup hook to run `org-trello-mode' when accessing files
  ;; ... in `org-trello-files'.
  (defun +boy--org-trello-helper ()
    (if (bound-and-true-p org-trello-mode)
        (remove-hook 'org-mode-hook #'boy--org-trello-helper)
      (progn
        (mapc (lambda (name)
                (when (string= (expand-file-name name) buffer-file-name)
                  (org-trello-mode)
                  (remove-hook 'org-mode-hook #'+boy--org-trello-helper)))
              org-trello-files))))
  (add-hook 'org-mode-hook #'+boy--org-trello-helper)
  :config
  (after! org-trello-setup
    ;; HACK: customize where the config files are stored
    (setq org-trello--config-dir (format "%s%s/" doom-cache-dir "trello")
          org-trello--config-file (concat org-trello--config-dir "%s.el"))
    (unless (file-directory-p org-trello--config-dir)
      (mkdir org-trello--config-dir t))
    ;; change default prefix
    (setq org-trello-default-prefix-keybinding "C-c l")
    (setq org-trello-current-prefix-keybinding "C-c l")))
