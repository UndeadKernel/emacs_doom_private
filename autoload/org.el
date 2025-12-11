;;; ~/.doom.d/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+boy/org-babel-hydra/body "~/.config/doom/autoload/org.el" nil t)
(defhydra +boy/org-babel-hydra (:hint nil)
    "
Org-Babel:
_n_:next      _c_:clear results  _i_:show all
_p_:previous  _h_:show/hide      _I_:hide all
_e_:edit      _RET_:execute      _l_:center screen
_g_:goto      _s_:split          _q_:cancel
"
    ("c" org-babel-remove-result)
    ("RET" org-babel-execute-src-block)
    ("e" org-edit-src-code)
    ("h" org-hide-block-toggle-maybe)
    ("s" org-babel-demarcate-block)
    ("g" org-babel-goto-named-src-block)
    ("i" org-show-block-all)
    ("I" org-hide-block-all)
    ("n" org-babel-next-src-block)
    ("p" org-babel-previous-src-block)
    ("l" recenter-top-bottom)
    ("q" nil :color blue))

;; Look in the arguments of source blocks for `:hidden' and hide those blocks
;; https://emacs.stackexchange.com/a/44923/9401
;;;###autoload
(defun +boy/hide-source-blocks-maybe ()
  "Fold blocks in the current buffer that have the argument `:hidden'."
  (interactive)
  (org-block-map
   (lambda ()
     (let ((case-fold-search t))
       (when (cl-assoc ':hidden (cl-third (org-babel-get-src-block-info)))
         (org-fold-hide-block-toggle t))))))

(defun +boy/hide-headings-maybe ()
  (interactive)
  (org-map-entries #'org-fold-hide-subtree "HIDDEN=\"t\""))

;; ORG PDF Annot functions

;;;###autoload
(defun +boy/save-pdf-and-notes (&optional arg)
  "When saving a PDF under the command of org-noter, save the associated org notes file."
  (interactive "p")
  (when (bound-and-true-p org-noter-doc-mode)
    ;; Save the associated notes file.
    (let ((notes-window (org-noter--get-notes-window)))
      (with-selected-window notes-window
        (save-buffer arg)))
  ;; Save the PDF file.
  (save-buffer arg)))

;;;###autoload
(defun +boy/highlight-and-annot-w-noter (list-of-edges)
  "Add a highlight with pdf-tools and an annotation with pdf-noter."
  (interactive (list (pdf-view-active-region nil)))
  (unless (pdf-view-active-region-p)
    (user-error "A selected PDF region is needed"))
  ;; highlights
  (let ((pdf-annot-activate-created-annotations nil))
    (pdf-annot-add-highlight-markup-annotation list-of-edges "snow3"))
  ;; add pdf-noter note
  (org-noter-insert-precise-note))

;;;###autoload
(defun +boy/highlight-and-add-item (list-of-edges indent)
  "Add a highlight and yank the highlighted text as a new list item."
  (interactive (list (pdf-view-active-region nil) current-prefix-arg))
  (unless (pdf-view-active-region-p)
    (user-error "A selected PDF region is needed"))
  ;; add new list item
  (let ((window (org-noter--get-notes-window 'force))
        (selected-text
         (replace-regexp-in-string "\n" " " (mapconcat #'identity
                                                       (pdf-view-active-region-text)
                                                       " "))))
    (select-frame-set-input-focus (window-frame window))
    (with-selected-window window
      (cond
       ;; point in a line with only an item bullet
       ((+boy/at-empty-item-p)
        (org-end-of-line)
        (insert selected-text)
        (fill-paragraph)
        (scroll-right))
       ;; point in an item
       ((org-in-item-p)
        (+org/insert-item-below 1)
        (insert selected-text)
        (fill-paragraph)
        (scroll-right))
       ;; point in a heading
       ((org-at-heading-p)
        (let ((headline-element (org-element-at-point)))
          (goto-char (org-element-property :contents-begin headline-element))
          (let ((props-element-maybe (org-element-at-point)))
            (when (eq 'property-drawer (car props-element-maybe))
              (goto-char (org-element-property :end props-element-maybe)))))
        (open-line 1)
        (insert "+ " selected-text)
        (fill-paragraph)
        (scroll-right))
       ;; point in a property drawer
       ((org-at-drawer-p)
        (let ((props-element-maybe (org-element-at-point)))
          (when (eq 'property-drawer (car props-element-maybe))
            (goto-char (org-element-property :end props-element-maybe))))
        (open-line 1)
        (insert "+ " selected-text)
        (fill-paragraph)
        (scroll-right))
       ;; point in an empty line or end of buffer
       ((or (string-match-p "\\`\\s-*$" (thing-at-point 'line)) (eobp))
        (insert "+ " selected-text)
        (fill-paragraph)
        (scroll-right))
       (t (display-warning :warning "Don't know where to add highlighted text. ")))))
  ;; add highlights
  (let ((pdf-annot-activate-created-annotations nil))
    (pdf-annot-add-highlight-markup-annotation list-of-edges "#eee8aa")))

;;;###autoload
(defun +boy/at-empty-item-p ()
  "Return t if point is at an empty list item."
  (string-match-p "^[ 	]*\\(\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)\\)[ 	]*$"
                  (thing-at-point 'line t)))

;; ORG Agenda functions

;;;###autoload
(defun +boy/is-project-p ()
  "Any task with a `PROJ' state"
  (member (org-get-todo-state) '("PROJ")))

;;;###autoload
(defun +boy/is-project-subtree-p ()
  "Any task with a todo keyword (that is not a project) and in a project subtree."
  (let ((is-proj nil))
    (save-excursion
      (org-back-to-heading 'invisible-ok)
      (while (and (not is-proj) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) '("PROJ"))
          (setq is-proj t)))
      is-proj)))

;;;###autoload
(defun +boy/is-task-p ()
  "Any task with a todo keyword that is not a project"
  (member (nth 2 (org-heading-components)) (remove "PROJ" org-todo-keywords-1)))

;;;###autoload
(defun +boy/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

;;;###autoload
(defun +boy/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

;;;###autoload
(defun +boy/toggle-next-task-display ()
  (interactive)
  (setq +boy/hide-scheduled-and-waiting-next-tasks (not +boy/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if +boy/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

;;;###autoload
(defun +boy/skip-stuck-projects ()
  "Skip stuck projects (i.e., projects without a todo or next task)"
  (save-restriction
    (widen)
    (let ((next-headline
           (save-excursion (or (outline-next-heading) (point-max)))))
      (if (+boy/is-project-p)
          (let* ((subtree-end
                  (save-excursion (org-end-of-subtree t)))
                 (has-next-or-todo))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next-or-todo)
                          (< (point) subtree-end)
                          (re-search-forward "^\\*+ \\(:?NEXT\\|TODO\\) " subtree-end t))
                (unless (member "WAIT" (org-get-tags))
                  (setq has-next-or-todo t))))
            (if has-next-or-todo
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

;;;###autoload
(defun +boy/skip-non-stuck-projects ()
  "Skip projects that are not stuck (i.e., no next or todo task)"
  (save-restriction
    (widen)
    (let ((next-headline
           (save-excursion (or (outline-next-heading) (point-max)))))
      (if (+boy/is-project-p)
          (let ((subtree-end (save-excursion (org-end-of-subtree t)))
                (has-next-or-todo nil))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next-or-todo)
                          (< (point) subtree-end)
                          (re-search-forward "^\\*+ \\(:?NEXT\\|TODO\\) " subtree-end t))
                (unless (member "WAIT" (org-get-tags))
                  (setq has-next-or-todo t))))
            (if has-next-or-todo
                next-headline
              nil)) ; a stuck project, has subtasks but no next or todo task
        next-headline))))

;;;###autoload
(defun +boy/skip-non-projects ()
  "Skip tasks that are not projects"
  ;; (+boy/list-sublevels-for-projects-indented)
  (if (save-excursion (+boy/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((+boy/is-project-p)
            nil)
           ((and (+boy/is-project-subtree-p) (not (+boy/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

;;;###autoload
(defun +boy/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((+boy/is-task-p)
        nil)
       (t
        next-headline)))))

;;;###autoload
(defun +boy/skip-projects-and-single-tasks ()
  "Skip trees that are projects, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((and +boy/hide-scheduled-and-waiting-next-tasks
             (or
              (member "WAIT" (org-get-tags))
              (save-excursion (re-search-forward org-scheduled-string next-headline t))))
        next-headline)
       ((+boy/is-project-p)
        next-headline)
       ((and (+boy/is-task-p) (not (+boy/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

;;;###autoload
(defun +boy/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((+boy/is-project-p)
        next-headline)
       ((and (not limit-to-project)
             (+boy/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (+boy/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun +boy/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((+boy/is-project-p)
        subtree-end)
       ((+boy/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun +boy/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((+boy/is-project-p)
        next-headline)
       ((not (+boy/is-project-subtree-p))
        subtree-end)
       ((and +boy/hide-scheduled-and-waiting-next-tasks
             (save-excursion (re-search-forward org-scheduled-string next-headline t)))
        next-headline)
       (t
        nil)))))

;;;###autoload
(defun +boy/skip-projects ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((+boy/is-project-p)
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun +boy/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;;;###autoload
(defun +boy/run-on-explorer (path _)
      "Open a \"explorer\" type link.
PATH is the path to open in explorer, as a string."
      (let ((command (concat "explorer.exe " (shell-quote-argument path)))
            ;; explorer.exe always returns an error code, so we supress all messages.
            (inhibit-message t) ;No messages in echo area.
            (message-log-max nil)) ; No messages in *Messages* buffer.
        (shell-command command))
      t)

;;;###autoload
(defun +boy/org-table-hline-p ()
  "Check if the point is on a horizontal line in an Org table."
  (and (org-at-table-p)
       (org-match-line "^[[:space:]]*|-")))
