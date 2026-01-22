;;; autoload/latex.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +boy/print-table-send-cmd ()
  "Insert the ORG command needed to create ORG tables."
  (interactive)
  (yas-expand-snippet (concat "#+ORGTBL: SEND ${1:name} orgtbl-to-latex :splice t :skip 2 :no-escape t" "\n" "$0")))

;;;###autoload
(defun +boy/print-table-rcv-cmd ()
  "Insert the ORG command needed to receive an ORG table."
  (interactive)
  (yas-expand-snippet (concat "% BEGIN RECEIVE ORGTBL ${1:table_name}" "\n"
                              "$0" "\n"
                              "% END RECEIVE ORGTBL $1")))

;;;###autoload
(defun +boy/latex-section (arg)
  "Call LaTeX-section temporarily changing TeX-grcl to add '%' after the section macro."
  (interactive "*P")
  (let ((TeX-grcl "}%"))
    (LaTeX-section arg)))

;;;###autoload
(defun +boy/run-latexmk ()
  "Run LatexMk without asking for confirmation. Saves the master file (and children)."
  (interactive)
  (TeX-save-document (TeX-master-file))
  (TeX-command "LatexMk" #'TeX-master-file -1))

;;;###autoload
(defun +boy/display-buffer-use-some-frame (fun &rest args)
  "Use `display-buffer-use-some-frame' as `display-buffer-overriding-action'.
Then run FUN with ARGS."
  (let ((display-buffer-overriding-action '(display-buffer-use-some-frame)))
    (apply fun args)))
