;;; private/boy/+org.el -*- lexical-binding: t; -*-

(defvar +boy/hide-scheduled-and-waiting-next-tasks t)

;; change `org-directory' only if it still has the default value
(if (or (not (boundp 'org-directory)) (string= org-directory "~/org"))
    (setq org-directory "~/documents/org/"))

;; Org archive configuration
(setq org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "roam/")
      +org-capture-notes-file "personal.org"
      +org-capture-journal-file "journal.org")

;; Org configuration
(after! org
  (setq org-startup-folded nil ; do not start folded
        org-tags-column 80 ; the column to the right to align tags
        org-log-done 'time ; record the time when an element was marked done/checked
        ;;org-fontify-done-headline nil ; do not change the font of DONE items
        org-fold-catch-invisible-edits 'show-and-error
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

  ;; Open PDF files in emacs
  (if (assoc "\\.pdf\\'" org-file-apps)
      (setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)
    (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs) t))

  ;; Do not move my buffer after cycling visibility
  (remove-hook 'org-cycle-hook #'org-cycle-optimize-window-after-visibility-change)

  ;; Capture templates

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

  ;; Agenda configuration

  (setq org-todo-state-tags-triggers
        '(("KILLED" ("KILLED" . t))
          ("WAIT" ("WAIT" . t))
          ("HOLD" ("WAIT") ("HOLD" . t))
          (done ("WAIT") ("HOLD"))
          ("TODO" ("WAIT") ("KILL") ("HOLD"))
          ("NEXT" ("WAIT") ("KILL") ("HOLD"))
          ("DONE" ("WAIT") ("KILL") ("HOLD"))))

  (setq org-todo-keywords
        '((sequence "PROJ(p)" "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "KILLED(k@/!)")))

  (setq org-agenda-compact-blocks nil ;; show separator lines between sections
        org-agenda-block-separator 8411 ;; emoji of dots on top of the line
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-start-day "" ;; show entries starting today
        org-agenda-span 15 ;; display from today, two weeks
        org-refile-target-verify-function '+boy/verify-refile-target
        org-agenda-dim-blocked-tasks nil)

  ;; Agenda helper functions

  (defun +boy/org-agenda-add-location-string ()
    "Gets the value of the LOCATION property"
    (let ((loc (org-entry-get (point) "LOCATION")))
      (if (> (length loc) 0)
          (concat "{" loc "} ")
        "")))

  ;; Agenda block definitions

  (defvar +boy--agenda-display-settings
    '((org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(+boy/org-agenda-add-location-string)% s")
                                  (timeline . "  % s")
                                  (todo . "  %-12:c")
                                  (tags . "  %-12:c")
                                  (search . "  %i %-12:c"))))
    "Display settings for agenda views.")

  (defvar +boy--agenda-block--two-weeks
    '(agenda "" ((org-agenda-overriding-header "Bi-Weekly Log")))
    "A block showing my schedule and logged tasks for the next two weeks.")

  (defvar +boy--agenda-block--today-schedule
    '(agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                 (org-agenda-span 'day)
                 (org-agenda-ndays 1)
                 (org-agenda-start-on-weekday nil)
                 (org-agenda-start-day "+0d")))
    "A block showing a 1 day schedule.")

  (defvar +boy--agenda-block--refile
    '(tags "REFILE"
      ((org-agenda-overriding-header "Tasks to Refile")
       (org-tags-match-list-sublevels nil)))
    "Headings needing refiling.")

  (defvar +boy--agenda-block--active-projects
    '(tags-todo "-HOLD-KILLED/!"
      ((org-agenda-overriding-header "Projects")
       (org-agenda-skip-function '+boy/skip-non-projects)
       (org-tags-match-list-sublevels 'indented)
       (org-agenda-sorting-strategy '(category-keep))))
    "All active projects.")

  (defvar +boy--agenda-block--inactive-projects
    '(tags-todo "-KILLED/!"
      ((org-agenda-overriding-header "Inactive Projects")
       (org-agenda-skip-function '+boy/skip-non-stuck-projects)
       (org-agenda-sorting-strategy '(category-keep))))
    "All inactive projects.")

  (defvar +boy--agenda-block--next-tasks
    '(tags-todo "-KILLED/!NEXT"
      ((org-agenda-overriding-header
        (concat "Next Tasks"
                (unless +boy/hide-scheduled-and-waiting-next-tasks
                  " (w/ WAIT and SCHEDULED tasks)")))
       (org-agenda-prefix-format " %-10c %-10(car (last (org-get-outline-path)))")
       (org-agenda-skip-function '+boy/skip-projects-and-single-tasks)
       (org-tags-match-list-sublevels 'indented)
       (org-agenda-todo-ignore-scheduled +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-deadlines +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-with-date +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
    "All next tasks.")

  (defvar +boy--agenda-block--project-subtasks
    '(tags-todo "-REFILE-KILLED-WAIT-HOLD/!"
      ((org-agenda-overriding-header
        (concat "Project Tasks"
                (unless +boy/hide-scheduled-and-waiting-next-tasks
                  " (w/ WAIT and SCHEDULED tasks)")))
       (org-agenda-prefix-format " %-10c %-10(car (last (org-get-outline-path)))")
       (org-agenda-skip-function '+boy/skip-non-project-tasks)
       (org-tags-match-list-sublevels 'indented)
       (org-agenda-todo-ignore-scheduled +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-deadlines +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-with-date +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-sorting-strategy '(category-keep))))
    "Tasks that belong to a project.")

  (defvar +boy--agenda-block--standalone-tasks
    '(tags-todo "-REFILE-KILLED-WAIT-HOLD/!"
      ((org-agenda-overriding-header
        (concat "Standalone Tasks"
                (unless +boy/hide-scheduled-and-waiting-next-tasks
                  " (w/ WAIT and SCHEDULED tasks)")))
       (org-agenda-skip-function '+boy/skip-project-tasks)
       (org-agenda-todo-ignore-scheduled +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-deadlines +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-with-date +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-sorting-strategy '(category-keep))))
    "All tasks that don't belong to a project.")

  (defvar +boy--agenda-block--inactive-tasks
    '(tags-todo "-KILLED+WAIT|HOLD/!"
      ((org-agenda-overriding-header
        (concat "Waiting and Postponed Tasks"
                (unless +boy/hide-scheduled-and-waiting-next-tasks
                  " (w/ WAIT and SCHEDULED tasks)")))
       (org-agenda-skip-function '+boy/skip-non-tasks)
       (org-tags-match-list-sublevels nil)
       (org-agenda-todo-ignore-scheduled +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-deadlines +boy/hide-scheduled-and-waiting-next-tasks)))
    "All tasks that are marked to be inactive.")

  ;; Agenda definitions

  (setq org-agenda-custom-commands
        `(("a" "Agenda Review (all)"
           (,+boy--agenda-block--two-weeks
            ,+boy--agenda-block--next-tasks
            ,+boy--agenda-block--refile
            ,+boy--agenda-block--project-subtasks
            ,+boy--agenda-block--standalone-tasks
            ,+boy--agenda-block--active-projects
            ,+boy--agenda-block--inactive-projects
            ,+boy--agenda-block--inactive-tasks)
           ,+boy--agenda-display-settings)
          ("t" "Today's Agenda"
           (,+boy--agenda-block--today-schedule
            ,+boy--agenda-block--refile
            ,+boy--agenda-block--next-tasks
            ,+boy--agenda-block--active-projects)
           ,+boy--agenda-display-settings)))

  ;; Agenda navigation

  (defun +boy/org-agenda-next-section ()
    "Go to the next section in an org agenda buffer."
    (interactive)
    (if (search-forward (char-to-string org-agenda-block-separator) nil t 1)
        (forward-line 1)
      (goto-char (point-max)))
    (beginning-of-line))

  (defun +boy/org-agenda-prev-section ()
    "Go to the next section in an org agenda buffer."
    (interactive)
    (forward-line -2)
    (if (search-forward (char-to-string org-agenda-block-separator) nil t -1)
        (forward-line 1)
      (goto-char (point-min))))

  ;; Agenda block removal

  (defun +boy/remove-agenda-regions ()
    (save-excursion
      (goto-char (point-min))
      (let ((region-large t))
        (while (and (< (point) (point-max)) region-large)
          (set-mark (point))
          (+boy/org-agenda-next-section)
          (if (< (- (region-end) (region-beginning)) 5) (setq region-large nil)
            (if (< (count-lines (region-beginning) (region-end)) 4)
                (delete-region (region-beginning) (region-end)))
            )))))
  (add-hook 'org-agenda-finalize-hook '+boy/remove-agenda-regions)

  ;; Noter config
  (setq org-noter-default-heading-title "Page $p$"
        org-noter-always-create-frame nil)
  ;; (add-hook! 'org-noter-notes-mode-hook (display-line-numbers-mode -1))

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
    ;; (setq-local org-image-actual-width '(300))
    )

  ;; Custom hack: Hide source blocks that have the attribute `:hidden'.
  (add-hook! 'org-mode-hook (+boy/hide-source-blocks-maybe))

  ;; Enable flyspell
  (add-hook 'org-mode-hook #'flyspell-mode)

  ;; After evaluating a SRC_BLOCK, redisplay inline images
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

  ;; Jupyter config
  (setq jupyter-eval-use-overlays t)

  ) ;; end of (after! org)
