;;; private/boy/+org.el -*- lexical-binding: t; -*-

(defvar +boy/hide-scheduled-and-waiting-next-tasks t)

;; change `org-directory' only if it still has the default value
(if (or (not (boundp 'org-directory)) (string= org-directory "~/org"))
  (setq org-directory "~/documents/org/"))

(setq org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "notes/")
      +org-capture-notes-file "personal.org"
      +org-capture-journal-file "journal.org")

;; ORG config
(after! org
  (setq org-startup-folded nil ; do not start folded
        org-tags-column 80 ; the column to the right to align tags
        org-log-done 'time ; record the time when an element was marked done/checked
        ;org-fontify-done-headline nil ; do not change the font of DONE items
        org-catch-invisible-edits 'show-and-error
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+"))
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")
        org-babel-min-lines-for-block-output 5 ; when to wrap results in #begin_example
        org-return-follows-link nil  ; RET doesn't follow links
        org-hide-emphasis-markers nil ; do show format markers
        org-startup-with-inline-images t ; open buffers show inline images
        ;; visual-fill-column-width 120 ; size for usage with visual fill column mode
        org-babel-default-header-args:sh '((:results . "verbatim"))
        org-todo-repeat-to-state t
        pdf-annot-activate-created-annotations nil ; do not open annotations after creating them
        org-duration-format (quote h:mm)) ; display clock times as hours only

  ;; open pdf files in emacs
  (if (assoc "\\.pdf\\'" org-file-apps)
      (setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)
    (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs) t))

    (setq org-todo-state-tags-triggers
          (quote (("KILLED" ("KILLED" . t))
                  ("WAIT" ("WAIT" . t))
                  ("HOLD" ("WAIT") ("HOLD" . t))
                  (done ("WAIT") ("HOLD"))
                  ("TODO" ("WAIT") ("KILL") ("HOLD"))
                  ("NEXT" ("WAIT") ("KILL") ("HOLD"))
                  ("DONE" ("WAIT") ("KILL") ("HOLD")))))

    (setq org-todo-keywords
          '((sequence "PROJ(p)" "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "KILLED(k@/!)")))

  ;; Do not move my buffer after cycling visibility
  (remove-hook 'org-cycle-hook #'org-optimize-window-after-visibility-change)

  ;; Custom org-capture templates
  (add-to-list 'org-capture-templates
               '("e" "Emacs Config Notes"
                 entry
                 (file+headline "emacs.org" "Notes")
                 "* %u %?\n %i\n %a"
                 :prepend t :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("w" "Work Templates"))
  (add-to-list 'org-capture-templates
               '("wn" "Notes"
                 entry
                 (file "refile.org")
                 "* %? :NOTE:\n%U"
                 :prepend t :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("wt" "Todo"
                 entry  ; type
                 (file "refile.org") ; target
                 "* TODO %?\n%U" ; template
                 :prepend t :kill-buffer t)) ; properties

  ;; Thesis finished, I don't need you anymore!!!
  ;; (add-to-list 'org-capture-templates
  ;;              '("h" "Templates for Thesis related info"))
  ;; (add-to-list 'org-capture-templates
  ;;              '("hP" "Thesis Check-out Pages"
  ;;                table-line  ; type
  ;;                (file "thesis.org") ; target
  ;;                "|%U||XXXXXXXX|%^{pages}|%^{comment}|" ; template
  ;;                :prepend t)) ; properties
  ;; (add-to-list 'org-capture-templates
  ;;              '("hp" "Thesis Check-in Pages"
  ;;                table-line  ; type
  ;;                (file "thesis.org") ; target
  ;;                "|%U|XXXXXXXX||%^{pages}|%^{comment}|" ; template
  ;;                :prepend t)) ; properties
  ;; (add-to-list 'org-capture-templates
  ;;              '("ht" "Thesis TODO"
  ;;                entry  ; type
  ;;                (file+headline "thesis.org" "TODOs") ; target
  ;;                "* [ ] %?\n%i" ; template
  ;;                :prepend t :kill-buffer t)) ; properties
  ;; (add-to-list 'org-capture-templates
  ;;              '("hT" "Thesis TODO w/link"
  ;;                entry  ; type
  ;;                (file+headline "thesis.org" "TODOs") ; target
  ;;                "* [ ] %?\nLINK: %l\n%i" ; template
  ;;                :prepend t :kill-buffer t)) ; properties
  ;; (add-to-list 'org-capture-templates
  ;;              '("hf" "Thesis FIXME"
  ;;                entry  ; type
  ;;                (file+headline "thesis.org" "FIXMEs") ; target
  ;;                "* [ ] %?\n%i" ; template
  ;;                :prepend t :kill-buffer t)) ; properties
  ;; (add-to-list 'org-capture-templates
  ;;              '("hF" "Thesis FIXME w/link"
  ;;                entry  ; type
  ;;                (file+headline "thesis.org" "FIXMEs") ; target
  ;;                "* [ ] %?\nLINK: %l\n%i" ; template
  ;;                :prepend t :kill-buffer t)) ; properties
  ;; (add-to-list 'org-capture-templates
  ;;              '("hn" "Thesis Note"
  ;;                entry  ; type
  ;;                (file+headline "thesis.org" "NOTEs") ; target
  ;;                "* %u %?\n%i" ; template
  ;;                :prepend t :kill-buffer t))
  ;; (add-to-list 'org-capture-templates
  ;;              '("hN" "Thesis Note w/link"
  ;;                entry  ; type
  ;;                (file+headline "thesis.org" "NOTEs") ; target
  ;;                "* %u %?\nLINK: %l\n%i" ; template
  ;;                :prepend t :kill-buffer t))

  ;; agenda config
  (setq org-agenda-compact-blocks t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-custom-commands
        '((" " "Agenda"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-KILLED/!"
                       ((org-agenda-overriding-header "Inactive Projects")
                        (org-agenda-skip-function '+boy/skip-non-stuck-projects)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-HOLD-KILLED/!"
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-skip-function '+boy/skip-non-projects)
                        (org-tags-match-list-sublevels 'indented)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-KILLED/!NEXT"
                       ((org-agenda-overriding-header
                         (concat "Project Next Tasks"
                                 (if +boy/hide-scheduled-and-waiting-next-tasks
                                     ""
                                   " (including WAIT and SCHEDULED tasks)")))
                        (org-agenda-skip-function '+boy/skip-projects-and-single-tasks)
                        (org-tags-match-list-sublevels 'indented)
                        (org-agenda-todo-ignore-scheduled +boy/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines +boy/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date +boy/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo "-REFILE-KILLED-WAIT-HOLD/!"
                       ((org-agenda-overriding-header (concat "Project Subtasks"
                                                              (if +boy/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAIT and SCHEDULED tasks)")))
                        (org-agenda-skip-function '+boy/skip-non-project-tasks)
                        (org-tags-match-list-sublevels 'indented)
                        (org-agenda-todo-ignore-scheduled +boy/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines +boy/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date +boy/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-REFILE-KILLED-WAIT-HOLD/!"
                       ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                              (if +boy/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAIT and SCHEDULED tasks)")))
                        (org-agenda-skip-function '+boy/skip-project-tasks)
                        (org-agenda-todo-ignore-scheduled +boy/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines +boy/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date +boy/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-KILLED+WAIT|HOLD/!"
                       ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                              (if +boy/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAIT and SCHEDULED tasks)")))
                        (org-agenda-skip-function '+boy/skip-non-tasks)
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-todo-ignore-scheduled +boy/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines +boy/hide-scheduled-and-waiting-next-tasks)))))))

  ;; org-noter config
  (setq org-noter-default-heading-title "Page $p$")
  ;; (add-hook! 'org-noter-notes-mode-hook (display-line-numbers-mode -1))

  (setq org-refile-target-verify-function '+boy/verify-refile-target))

;; Disable line numbers by default
(add-hook! 'org-mode-hook (display-line-numbers-mode -1))

;; Enable displaying of inline PDF images in ORG files
;; https://stackoverflow.com/a/35261577/2632102
(add-hook! 'org-mode-hook
  (make-local-variable 'image-type-file-name-regexps)
  (make-local-variable 'image-file-name-extensions)
  (add-to-list 'image-type-file-name-regexps '("\\.pdf\\'" . imagemagick))
  (add-to-list 'image-file-name-extensions "pdf")
  (setq-local imagemagick-types-inhibit (remove 'PDF imagemagick-types-inhibit))
  (setq-local org-image-actual-width '(300)))

;; Custom hack: Hide source blocks that have the attribute `:hidden'.
(add-hook! 'org-mode-hook (+boy/hide-source-blocks-maybe))

;; Enable flyspell
(add-hook 'org-mode-hook #'flyspell-mode)

;; After evaluating a SRC_BLOCK, redisplay inline images
(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

;; (after! org
;;   (defun org-babel-result-end ()
;;   "Return the point at the end of the current set of results."
;;   (cond ((looking-at-p "^[ \t]*$") (point)) ;no result
;;    ((looking-at-p (format "^[ \t]*%s[ \t]*$" org-bracket-link-regexp))
;;     (line-beginning-position 2))
;;     (t (save-excursion
;;       (goto-char
;;        (min (point-max)    ;for narrowed buffers
;;           (org-element-property :end (org-element-at-point))))
;;       (skip-chars-backward " \r\t\n")
;;       (line-beginning-position 2))))))

(after! org
  (mapc (lambda (color)
          (add-to-list 'org-tag-faces `(,color . (:foreground ,color))))
        '("yellow" "green" "red" "blue" "purple" "orange")))
