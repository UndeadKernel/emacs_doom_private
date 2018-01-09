;;; config.el --- The Configuration of the Boy

(defvar boy--synonyms-key ""
  "API key from http://thesaurus.altervista.org that gives us synonyms.")

(load! +windows)
(load! +bindings)
(load! +functions)
(load! +latex)

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

