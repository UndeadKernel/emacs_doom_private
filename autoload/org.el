;;; ~/.doom.d/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+boy/org-babel-hydra/body "~/.doom.d/autoload/org.el" nil t)
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
         (org-hide-block-toggle))))))

;; ORG PDF Annot functions

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
       ;; point in an empty line
       ((string-match-p "\\`\\s-*$" (thing-at-point 'line))
        (insert "+ " selected-text) )
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
       (t (display-warning :warning "Don't know where to add highlighted text. ")))))
  ;; add highlights
  (let ((pdf-annot-activate-created-annotations nil))
    (pdf-annot-add-highlight-markup-annotation list-of-edges "#eee8aa")))

(defun +boy/at-empty-item-p ()
  "Return t if point is at an empty list item."
  (string-match-p "^[ 	]*\\(\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)\\)[ 	]*$"
                  (thing-at-point 'line t)))

;; ORG Agenda functions

;;;###autoload
(defun +boy/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

;;;###autoload
(defun +boy/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

;;;###autoload
(defun +boy/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (+boy/find-project-task)
      (if (equal (point) task)
          nil
        t))))

;;;###autoload
(defun +boy/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

;;;###autoload
(defun +boy/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

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
  "Skip trees that are stuck projects (i.e., subtrees without a todo or next task)"
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
  "Skip trees that are not stuck projects (i.e., no next or todo task)"
  (save-restriction
    (widen)
    (let ((next-headline
           (save-excursion (or (outline-next-heading) (point-max)))))
      (if (+boy/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next-or-todo ))
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
  "Skip trees that are not projects"
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
(defun +boy/skip-project-trees ()
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
       ((and (+boy/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT" "WAIT" "HOLD")))
        subtree-end)
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
(defun +boy/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (+boy/is-subproject-p)
        nil
      next-headline)))

;;;###autoload
(defun +boy/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
