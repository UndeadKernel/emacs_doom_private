;;; app/bibliography/autoload.el -*- lexical-binding: t; -*-

(defvar +bibliography--wconf nil
  "Hold the window configuration before `+bibliography/init' is invoked
(when workspaces are not used).")

;;;###autoload
(defun =bibliography ()
  "Activate (or switch to) the `bibliography' in its workspace."
  (interactive)
  (if (featurep! :ui workspaces)
      (progn
        (+workspace-switch +bibliography-workspace-name t)
        (+workspace/display))
    (setq +bibliography--wconf (current-window-configuration))
    (delete-other-windows))
  (+bibliography/init))

(defun +bibliography/init ()
  (let* ((file-path (concat +bibliography-notes-dir +bibliography-notes-file))
         (file-exists (file-exists-p file-path)))
    (find-file file-path)
    (unless file-exists
      ;; set the title of the org buffer
      (insert "Bibliography\n\n")
      (save-buffer))
    ;; enable Zotero integration
    (org-zotxt-mode)))

;;;###autoload
(defun +bibliography/quit ()
  "Delete the `bibliography' buffers and remove the workspace (if any)."
  (interactive)
  (if (featurep! :ui workspaces)
      (+workspace/delete +bibliography-workspace-name)
    (doom-kill-matching-buffers (concat "^\\" +bibliography-notes-file))
    (set-window-configuration +bibliography--wconf)
    (setq +bibliography--wconf nil)))
