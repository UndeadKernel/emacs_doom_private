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
(defun +boy/org-agenda-next-section ()
  "Go to the next section in an org agenda buffer."
  (interactive)
  (if (search-forward (char-to-string org-agenda-block-separator) nil t 1)
      (forward-line 1)
    (goto-char (point-max)))
  (beginning-of-line))

;;;###autoload
(defun +boy/org-agenda-prev-section ()
  "Go to the previous section in an org agenda buffer."
  (interactive)
  (forward-line -2)
  (if (search-forward (char-to-string org-agenda-block-separator) nil t -1)
      (forward-line 1)
    (goto-char (point-min))))

;;;###autoload
(defun +boy/remove-agenda-regions ()
  "Go through the main agenda regions, deleting them if it they are small enough."
  (when (string= org-agenda-name "Agenda Review (all)")
    (save-excursion
      (goto-char (point-min))
      (let ((region-large t))
        (while (and (< (point) (point-max)) region-large)
          (set-mark (point))
          (+boy/org-agenda-next-section)
          (if (< (- (region-end) (region-beginning)) 5) (setq region-large nil)
            (if (< (count-lines (region-beginning) (region-end)) 4)
                (delete-region (region-beginning) (region-end)))))))))

;;;###autoload
(defun +boy/remove-refile-tag ()
  "Remove the REFILE tag of the headline with the current point."
  (org-set-tags (seq-filter (lambda (elt) (not (string= elt "REFILE"))) (org-get-tags))))
(add-hook! 'org-after-refile-insert-hook #'+boy/remove-refile-tag)

;;;###autoload
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

;;;###autoload
(defun +boy/update-meeting-actions-count ()
  "Check if a Meeting headline child has pending actions."
  (interactive)
  (org-with-wide-buffer
   (save-excursion
     (goto-char (point-min))
     (let ((meeting-found nil))
       ;; Iterate over all "Meetings" headlines
       (while (re-search-forward (format org-complex-heading-regexp-format "Meetings") nil t)
         (setq meeting-found (org-goto-first-child)) ; go to the first meeting headline
         ;; Iterate each meeting (child of the Meetings headline).
         (while meeting-found
           (let ((meeting-element (org-element-at-point))
                 (meeting-todo (org-entry-is-todo-p)) ; track if the meeting itself has an active todo
                 (actions-count (length (org-map-entries t "/!-DONE" 'tree))))
             ;; Change the ACTIONS property to `todo-count'. Because `org-map-entries' counts the
             ;; ... todo of the parent headline, if one is found in the parent, substract it.
             (org-entry-put meeting-element "ACTIONS"
                            (format "%d" (- actions-count (if meeting-todo 1 0)))))
           (setq meeting-found (org-goto-sibling))))))))

;;;###autoload
(defun +boy/color-completion (&optional _)
  "Provide completion candidates for the `color' link type."
  (let ((color-data (progn
                      (save-selected-window
                        (list-colors-display))
                      (append
                       (mapcar (lambda (cell)
                                 (cons (propertize (car cell) 'face (cdr cell))
                                       (list (car cell))))
                               +boy--org-link-colors)
                       (prog1
                           (with-current-buffer (get-buffer "*Colors*")
                             (mapcar (lambda (line)
                                       (append (list line) (s-split " " line t)))
                                     (s-split "\n" (buffer-string))))
                         (kill-buffer "*Colors*"))))))
    (format "color:%s"
            (s-trim (cadr (assoc (completing-read "Color: " color-data) color-data))))))

;;;###autoload
(defun +boy/color-link-face (path)
  "Return a color face, prioritizing faces from `+boy--org-link-colors'."
  (or (cdr (assoc path +boy--org-link-colors))
      `(:foreground ,path)))

;;;###autoload
(defun +boy/color-link-export (path description backend)
  "Export colo red links by creating a <span> with the desired color."
  (cond
   ((eq backend 'html)
    (let ((rgb (assoc (downcase path) color-name-rgb-alist))
          r g b)
      (if rgb
          (progn
            (setq r (* 255 (/ (nth 1 rgb) 65535.0))
                  g (* 255 (/ (nth 2 rgb) 65535.0))
                  b (* 255 (/ (nth 3 rgb) 65535.0)))
            (format "<span style=\"color: rgb(%s,%s,%s)\">%s</span>"
                    (truncate r) (truncate g) (truncate b)
                    (or description path)))
        (format "No Color RGB for %s" path))))))

;;;###autoload
(defun +boy/org-roam-find-acronyms ()
  "Get the file path of the `acronyms' node using roam."
  (org-roam-node-file (org-roam-node-from-title-or-alias "acronyms")))

;;;###autoload
(defun +boy/org-roam-find-people ()
  "Get the file path of the `people' node using roam."
  (org-roam-node-file (org-roam-node-from-title-or-alias "people")))
