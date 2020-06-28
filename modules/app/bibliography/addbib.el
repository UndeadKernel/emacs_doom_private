;;; app/bibliography/addbib.el -*- lexical-binding: t; -*-

(require 'cl-lib)


(defvar +bibliography--add-bib-buffer-name "*new bibs*")
(defvar +bibliography--add-bib-buffer nil)


(defun +bibliography/add-bib ()
  "Open a window where a user can add .bib entries to be parsed and added."
  (interactive)
  (select-window
   (display-buffer
    (if (buffer-live-p +bibliography--add-bib-buffer)
        +bibliography--add-bib-buffer
      (setq +bibliography--add-bib-buffer (get-buffer-create +bibliography--add-bib-buffer-name))
      (with-current-buffer +bibliography--add-bib-buffer
        (+bibliography--add-bib-minor-mode 1)
        (current-buffer))))))

(defvar +bibliography--add-bib-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap text-mode-map)
    (define-key kmap (kbd "C-c C-c") '+bibliography--add-bib-commit)
    (define-key kmap (kbd "C-c C-q") '+bibliography--add-bib-abort)
    kmap)
  "Mode map used when adding a bib entry.")

(define-minor-mode +bibliography--add-bib-minor-mode
  "Active when in a buffer where bib entries can be added."
  nil nil nil
  (when +bibliography--add-bib-minor-mode
    (setq header-line-format
          (format  "%s"
                   (substitute-command-keys
                    (concat
                     "\\[+bibliography--add-bib-commit] to commit, "
                     "\\[+bibliography--add-bib-abort] to abort."))))))

(put '+bibliography--add-bib-minor-mode 'permanent-local t)

(defun +bibliography--add-bib-commit ()
  (interactive)
  (+bibliography--add-bib-finalize t))

(defun +bibliography--add-bib-abort ()
  (interactive)
  (+bibliography--add-bib-finalize nil))

(defun +bibliography--add-bib-finalize (save)
  (when (and (buffer-modified-p) save)
    (+bibliography--add-bib-save)
    (set-buffer-modified-p nil))
  (kill-buffer-and-window))

(defun +bibliography--add-bib-save ()
  "Do the parsing and inclusion of the user-supplied bib entries."
  ;; add bib entries to bibliography file
  (unless +bibliography--buffer
    (user-error "Bibliography file not open."))
  (message "Saving bib entries...")
  (goto-char (point-min))
  (setq entries (make-hash-table :test #'equal))
  (setq strings (make-hash-table :test #'equal))

  ;; parse for bib entries
  (cl-loop for item = (+bibliography-find-next-item)
           while item do
           (cond
            ((cl-equalp item "string") ; `cl-equalp' compares strings case-insensitively.
             (let ((string (+bibliography-read-string nil strings)))
               (if string
                   (puthash (car string) (cdr string) strings))))
            ((stringp item)
             (let ((entry (+bibliography-read-entry item nil strings)))
               (when entry
                 (puthash (cdr (assoc-string "=key=" entry)) entry entries))))))

  ;; add bib entries
  (with-current-buffer +bibliography--buffer
    (goto-char (point-max))
    (unless (and (bolp) (eolp))
      (insert "\n"))
    (maphash (lambda (key value)
               (insert (concat "\n* " (alist-get "title" value "No Title" nil #'string=)))
               (save-excursion
                 (insert "\n:PROPERTIES:")
                 (insert "\n:Author: " (alist-get "author" value "NA" nil #'string=))
                 (insert "\n:Year: " (alist-get "year" value "NA" nil #'string=))
                 (insert "\n:END:")
                 (insert "\n#+begin_src bibtex")
                 (insert "\n" (alist-get "=source=" value "" nil #'string=))
                 (insert "\n#+end_src")
                 (insert "\n** [[pdf:pdfs/" key ".pdf::1][Notes]]")
                 (insert "\n:PROPERTIES:")
                 (insert "\n:NOTER_DOCUMENT: pdfs/" key ".pdf")
                 (insert "\n:END:"))
               (outline-hide-subtree)
               (org-set-startup-visibility))
             entries)))
