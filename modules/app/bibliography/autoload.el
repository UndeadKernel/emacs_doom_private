;;; app/bibliography/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =bibliography ()
  "Activate (or switch to) the `bibliography' in its workspace."
  (interactive)
  (load! "parser")
  (load! "addbib")
  (if (featurep! :ui workspaces)
      (progn
        (+workspace-switch +bibliography-workspace-name t)
        (+workspace/display))
    (setq +bibliography--wconf (current-window-configuration))
    (delete-other-windows))
  (+bibliography/init))

;;;###autoload
(defun +bibliography/init ()
  (let* ((file-path (concat +bibliography-notes-dir +bibliography-notes-file))
         (file-exists (file-exists-p file-path)))
    (find-file file-path)
    (unless file-exists
      ;; set the title of the org buffer
      (insert "Bibliography\n")
      (insert "#+COLUMNS: %60ITEM %TAGS %AUTHOR %YEAR")
      (insert (concat "#+PROPERTY: header-args:bibtex :exports none :tangle "
                      "\"" +bibliography-bib-file "\"\n"))
      (insert "#+STARTUP: hideblocks overview\n\n")
      (save-buffer))
    (setq +bibliography--buffer (current-buffer))))

;;;###autoload
(defun +bibliography/quit ()
  "Delete the `bibliography' buffers and remove the workspace (if any)."
  (interactive)
  (if (featurep! :ui workspaces)
      (+workspace/delete +bibliography-workspace-name)
    (kill-buffer +bibliography--buffer)
    (set-window-configuration +bibliography--wconf)
    (setq +bibliography--wconf nil))
  (setq +bibliography--buffer nil))
