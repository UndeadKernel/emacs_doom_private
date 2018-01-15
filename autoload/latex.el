;;; ../../../doom_private/autoload/latex.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +boy/print-table-send-cmd ()
  "Insert the ORG command needed to create ORG tables."
  (interactive)
  (yas-expand-snippet "#+ORGTBL: SEND ${1:name} orgtbl-to-latex :splice t :skip 2 :no-escape t\n$0"))

;;;###autoload
(defun +boy/print-table-rcv-cmd ()
  "Insert the ORG command needed to receive an ORG table."
  (interactive)
  (yas-expand-snippet (concat "% BEGIN RECEIVE ORGTBL ${1:table_name}" "\n"
                              "$0" "\n"
                              "% END RECEIVE ORGTBL $1")))
