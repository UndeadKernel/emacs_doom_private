;;; lang/org-hermes/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(define-minor-mode org-hermes-mode
  "Send Org http and https links to a remote org-hermes opener.

When enabled, this mode overrides Org's http and https link follow functions.
When disabled, it restores the exact previous Org link parameters captured when
this mode was first enabled."
  :global t
  :group 'org-hermes
  (if org-hermes-mode
      (progn
        ;; Save the previous follow functions
        (unless org-hermes--previous-http-parameters
          (setq org-hermes--previous-http-parameters
                (copy-sequence (org-hermes--link-parameters "http"))))
        (unless org-hermes--previous-https-parameters
          (setq org-hermes--previous-https-parameters
                (copy-sequence (org-hermes--link-parameters "https"))))
        ;; Set our follow functions
        (org-link-set-parameters "http"
                                 :follow #'org-hermes--http-follow)
        (org-link-set-parameters "https"
                                 :follow #'org-hermes--https-follow))
    (when org-hermes--previous-http-parameters
      (org-hermes--set-link-parameters
       "http" org-hermes--previous-http-parameters)
      (setq org-hermes--previous-http-parameters nil))
    (when org-hermes--previous-https-parameters
      (org-hermes--set-link-parameters
       "https" org-hermes--previous-https-parameters))
      (setq org-hermes--previous-https-parameters nil)))
