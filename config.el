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

;; Don't save undo-tree history
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;; (after! undo-tree
;;     (advice-remove #'undo-tree-make-history-save-file-name #'doom*undo-tree-make-history-save-file-name)
;;     (advice-remove #'undo-list-transfer-to-tree #'doom*strip-text-properties-from-undo-history)
;;     (advice-remove #'undo-tree-save-history #'doom*compress-undo-tree-history)
;;     (advice-remove #'undo-tree-load-history #'doom*shut-up))

;; (after! undo-tree
;;   (advice-remove #'undo-tree-make-history-save-file-name #'doom*undo-tree-make-history-save-file-name))

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

(after! ibuffer
  ;; nearly all of this is the default layout
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 50 50 :left :elide) ; change: 30s were originally 18s
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))

;; disable :unless predicates with (sp-pair "'" nil :unless nil)
;; disable :post-handlers with (sp-pair "{" nil :post-handlers nil)
;; ...or specific :post-handlers with (sp-pair "{" nil :post-handlers '(:rem ("| " "SPC")))

;; (after! smartparens
;;   ;; Autopair quotes more conservatively; if I'm next to a word/before another
;;   ;; quote, I likely don't want another pair.
;;   (let ((unless-list '(sp-point-before-word-p
;;                        sp-point-after-word-p
;;                        sp-point-before-same-p)))
;;     (sp-pair "'"  nil :unless unless-list)
;;     (sp-pair "\"" nil :unless unless-list))

;;   ;; Expand {|} => { | }
;;   ;; Expand {|} => {
;;   ;;   |
;;   ;; }
;;   (dolist (brace '("(" "{" "["))
;;     (sp-pair brace nil
;;              :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
;;              ;; I likely don't want a new pair if adjacent to a word or opening brace
;;              :unless '(sp-point-before-word-p sp-point-before-same-p)))

;;   ;; Don't do square-bracket space-expansion where it doesn't make sense to
;;   (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
;;                  "[" nil :post-handlers '(:rem ("| " "SPC")))

;;   ;; Highjacks backspace to:
;;   ;;  a) balance spaces inside brackets/parentheses ( | ) -> (|)
;;   ;;  b) delete space-indented `tab-width' steps at a time
;;   ;;  c) close empty multiline brace blocks in one step:
;;   ;;     {
;;   ;;     |
;;   ;;     }
;;   ;;     becomes {|}
;;   ;;  d) refresh smartparens' :post-handlers, so SPC and RET expansions work
;;   ;;     even after a backspace.
;;   ;;  e) properly delete smartparen pairs when they are encountered, without the
;;   ;;     need for strict mode.
;;   ;;  f) do none of this when inside a string
;;   (advice-add #'delete-backward-char :override #'doom/delete-backward-char))
