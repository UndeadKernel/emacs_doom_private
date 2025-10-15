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
        ;;org-tags-column 120 ; the column to the right to align tags
        org-log-done 'time ; record the time when an element was marked done/checked
        ;;org-fontify-done-headline nil ; do not change the font of DONE items
        org-fold-catch-invisible-edits 'show-and-error
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+"))
        ;; org-ellipsis "  "
        org-ellipsis " "
        org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")
        org-babel-min-lines-for-block-output 5 ; when to wrap results in #begin_example
        org-return-follows-link nil  ; RET doesn't follow links
        ;; org-hide-emphasis-markers nil ; do show format markers
        org-startup-with-inline-images t ; open buffers show inline images
        ;; visual-fill-column-width 120 ; size for usage with visual fill column mode
        org-babel-default-header-args:sh '((:results . "verbatim"))
        org-todo-repeat-to-state t
        pdf-annot-activate-created-annotations nil ; do not open annotations after creating them
        org-id-method 'ts ; create IDs using time
        org-id-ts-format "id-%Y%m%d-%H%M%S" ; the format of created IDs
        org-cycle-separator-lines -1 ; leave empty lines between collapsed headlines
        org-modern-table-vertical 1 ; pretty vertical lines in tables 1px in width
        org-agenda-tags-todo-honor-ignore-options t ; don't show SCHEDULED items in agenda view
        org-deadline-warning-days 0 ; don't inform me that a deadline is coming
        org-reverse-note-order t ; add captured notes to the beginning (and not the end)
        )

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
                 "* %? (org-set-property \"DATE\" \"%U\")\n %i\n %a"
                 :prepend t :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("w" "Work Templates"))
  (add-to-list 'org-capture-templates
               '("wm" "Meeting"
                 entry
                 (file+headline "refile.org" "Meetings")
                 "*** %? %(org-set-property \"DATE\" \"%U\")\n**** Participants\n**** Notes\n**** Actions [/]\n***** No actions\n" ; template
                 :prepend t :clock-in t :clock-keep t :jump-to-captured t))
  (add-to-list 'org-capture-templates
               '("wt" "Tasks"
                 entry  ; type
                 (file+headline "refile.org" "Tasks") ; target
                 "* TODO %?  %(org-set-tags \"REFILE\") %(org-set-property \"DATE\" \"%U\")\n" ; template
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

  (defun +boy/remove-refile-tag ()
    "Remove the REFILE tag of the headline with the current point."
    (org-set-tags (seq-filter (lambda (elt) (not (string= elt "REFILE"))) (org-get-tags))))
  (add-hook! 'org-after-refile-insert-hook #'+boy/remove-refile-tag)

  ;; Agenda helper functions

  (defun +boy/org-agenda-get-proj-maybe ()
    "Gets the value of the LOCATION property"
    (let ((heading-found
           (catch 'heading
             (save-excursion
               (while (org-up-heading-safe)
                 (if (string= (org-get-todo-state) "PROJ")
                     (progn
                       (throw 'heading (org-get-heading t t t t)))))))))
      (if heading-found
          (let ((heading-size (length heading-found)))
            (substring heading-found 0 (min heading-size 6)))
        "")))

  ;; Agenda block definitions

  (defvar +boy--agenda-display-settings
    '((org-agenda-prefix-format '((agenda . "  %-12c%?-12t%s %?-8(+boy/org-agenda-get-proj-maybe)")
                                  (timeline . "  % s")
                                  (todo . "  %-12c")
                                  (tags . "  %-12c")
                                  (search . "  %i %-12c"))))
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
      ((org-agenda-overriding-header "Items to Refile")
       (org-agenda-prefix-format "  %-12c%-8(+boy/org-agenda-get-proj-maybe)")
       (org-tags-match-list-sublevels t)))
    "Headings needing refiling.")

  (defvar +boy--agenda-block--active-projects
    '(tags-todo "-HOLD-KILLED/!"
      ((org-agenda-overriding-header "Projects")
       (org-agenda-skip-function '+boy/skip-non-projects)
       (org-tags-match-list-sublevels t)
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
       (org-agenda-prefix-format "  %-12c%-8(+boy/org-agenda-get-proj-maybe)")
       (org-agenda-skip-function '+boy/skip-non-tasks)
       (org-tags-match-list-sublevels t)
       (org-agenda-todo-ignore-scheduled +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-deadlines +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-with-date +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-sorting-strategy '(category-keep))))
    "All next tasks.")

  (defvar +boy--agenda-block--project-subtasks
    '(tags-todo "-REFILE-KILLED-WAIT-HOLD/!-NEXT"
      ((org-agenda-overriding-header
        (concat "Project Tasks"
                (unless +boy/hide-scheduled-and-waiting-next-tasks
                  " (w/ WAIT and SCHEDULED tasks)")))
       (org-agenda-prefix-format "  %-12c%-8(+boy/org-agenda-get-proj-maybe)")
       (org-agenda-skip-function '+boy/skip-non-project-tasks)
       (org-tags-match-list-sublevels t)
       (org-agenda-todo-ignore-scheduled +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-deadlines +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-with-date +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-sorting-strategy '(category-keep))))
    "Tasks that belong to a project.")

  (defvar +boy--agenda-block--standalone-tasks
    '(tags-todo "-REFILE-KILLED/!-NEXT"
      ((org-agenda-overriding-header
        (concat "Standalone Tasks"
                (unless +boy/hide-scheduled-and-waiting-next-tasks
                  " (w/ WAIT and SCHEDULED tasks)")))
       (org-agenda-prefix-format "  %-12c%-8(+boy/org-agenda-get-proj-maybe)")
       (org-agenda-skip-function '+boy/skip-project-tasks)
       (org-agenda-todo-ignore-scheduled +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-deadlines +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-todo-ignore-with-date +boy/hide-scheduled-and-waiting-next-tasks)
       (org-agenda-sorting-strategy '(category-keep))))
    "All tasks that don't belong to a project.")

  (defvar +boy--agenda-block--inactive-tasks
    '(tags-todo "-KILLED/!+HOLD|+WAIT"
      ((org-agenda-overriding-header
        (concat "Inactive Tasks"
                (unless +boy/hide-scheduled-and-waiting-next-tasks
                  " (w/ WAIT and SCHEDULED tasks)")))
       (org-agenda-prefix-format "  %-12c%-8(+boy/org-agenda-get-proj-maybe)")
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
            ,+boy--agenda-block--inactive-tasks
            ,+boy--agenda-block--active-projects
            ,+boy--agenda-block--inactive-projects)
           ,+boy--agenda-display-settings)
          ("t" "Today's Agenda"
           (,+boy--agenda-block--today-schedule
            ,+boy--agenda-block--next-tasks
            ,+boy--agenda-block--refile
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

  ;; Hide source blocks with the attribute `:hidden' and with property `:HIDDEN: t'.
  (add-hook! 'org-mode-hook '(+boy/hide-source-blocks-maybe +boy/hide-headings-maybe))

  ;; Enable flyspell
  (add-hook 'org-mode-hook #'flyspell-mode)

  ;; After evaluating a SRC_BLOCK, redisplay inline images
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

  ;; Jupyter config
  (setq jupyter-eval-use-overlays t)

  ) ;; end of (after! org)

;; Configure things related to tables in org.
(after! org-table
  ;; Disable prettifying tables with org-modern if showing the coordinates overlay.
  (advice-add #'org-table-toggle-coordinate-overlays
              :after (lambda ()
                       (if org-table-overlay-coordinates
                           (font-lock-unfontify-region (org-table-begin) (org-table-end))
                         (font-lock-fontify-region (org-table-begin) (org-table-end))))))
