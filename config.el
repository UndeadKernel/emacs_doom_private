;;; config.el --- The Configuration of the Boy

(defvar boy--synonyms-key ""
  "API key from http://thesaurus.altervista.org that gives us synonyms.")

(load! +windows)
(load! +bindings)
(load! +functions)
(load! +latex)
(load! +modeline)
(load! +popups)

;; Smooth mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))  ; scroll two lines at a time
      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
      mouse-wheel-follow-mouse t                    ; scroll window under mouse
      scroll-step 1)

;; Backups
(setq backup-by-copying t      ; No symbolic links
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Show trailing white spaces
(setq show-trailing-whitespace t)
(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

;; Show me where I made the last change in a document.
(def-package! goto-last-change
  :commands goto-last-change)

;; Resize windows interactively.
(def-package! resize-window
  :commands (resize-window))

;; Latex synonyms
(def-package! www-synonyms
  :if (s-present? boy--synonyms-key)
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

(after! smartparens
  ;; Auto-close more conservatively and expand braces on RET
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))

;; EIN config
(set! :ein-notebook-dir "~/Documents/CASED/Development")
(add-hook! ein:notebook-multilang-mode
  (map! :map ein:notebook-mode-map
        "M-p" #'+boy/up-scroll
        "M-n" #'+boy/down-scroll))

