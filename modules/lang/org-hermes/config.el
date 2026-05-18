;;; lang/org-hermes/config.el --- Open Org HTTP links on a remote desktop -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure Org so http:// and https:// links opened with `org-open-at-point'
;; are sent to an org-hermes HTTP endpoint, usually exposed from a remote
;; host to this machine through an SSH reverse tunnel.
;;
;; If the remote opener cannot be reached or reports failure, links fall back to
;; the normal local Org/browser behavior.
;;
;; Minimal setup:
;;
;;   (require 'org-hermes)
;;   (org-hermes-mode 1)
;;
;; Optional endpoint override:
;;
;;   (setq org-hermes-endpoint "http://127.0.0.1:9876/open")

;;; Code:

(require 'json)
(require 'org)
(require 'org-element)
(require 'ol)
(require 'url-http)

(defgroup org-hermes nil
  "Open Org HTTP links through a remote org-hermes opener."
  :group 'org
  :prefix "org-hermes-")

(defcustom org-hermes-endpoint "http://127.0.0.1:9876/open"
  "HTTP endpoint that accepts POST /open JSON requests.

The endpoint should accept a JSON body of the form:

  {\"url\":\"https://example.com\"}

and return a 2xx HTTP response when the URL was opened remotely."
  :type 'string
  :group 'org-hermes)

(defcustom org-hermes-timeout 3
  "Maximum number of seconds to wait for the remote opener."
  :type 'number
  :group 'org-hermes)

(defcustom org-hermes-fallback-to-local t
  "When non-nil, open links locally if remote opening fails."
  :type 'boolean
  :group 'org-hermes)

(defcustom org-hermes-notify-on-fallback t
  "When non-nil, show a message when falling back to local opening."
  :type 'boolean
  :group 'org-hermes)

(defvar org-hermes--previous-http-parameters nil
  "Previous Org parameters for the http link type.")

(defvar org-hermes--previous-https-parameters nil
  "Previous Org parameters for the https link type.")

(defun org-hermes--link-parameters (type)
  "Return Org link parameter plist for TYPE."
  (cdr (assoc type org-link-parameters)))

(defun org-hermes--set-link-parameters (type parameters)
  "Set TYPE's Org link PARAMETERS, replacing existing parameters."
  (let ((entry (assoc type org-link-parameters)))
    (if entry
        (setcdr entry parameters)
      (push (cons type parameters) org-link-parameters)
      (org-link-make-regexps)
      (when (featurep 'org-element)
        (org-element-update-syntax)))))

(defun org-hermes--url (scheme path)
  "Build a full URL from SCHEME and Org link PATH."
  (concat scheme ":" path))

(defun org-hermes--encode-request (url)
  "Return JSON request body for URL."
  (json-encode `((url . ,url))))

(defun org-hermes--buffer-http-success-p ()
  "Return non-nil when current `url-retrieve-synchronously' buffer is HTTP 2xx."
  (goto-char (point-min))
  (and (re-search-forward "^HTTP/[0-9.]+ +\\([0-9]+\\)" nil t)
       (let ((status (string-to-number (match-string 1))))
         (and (>= status 200) (< status 300)))))

(defun org-hermes-open-url-remotely (url)
  "Open URL through `org-hermes-endpoint'.

Return non-nil if the remote endpoint accepted the request.  Signal an error
when the endpoint is unreachable or returns a non-2xx response."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
                            (org-hermes--encode-request url)
                            'utf-8))
         (buffer (url-retrieve-synchronously org-hermes-endpoint
                                             nil nil org-hermes-timeout)))
    (unless buffer
      (error "The org-hermes endpoint did not respond within %s seconds"
             org-hermes-timeout))
    (unwind-protect
        (with-current-buffer buffer
          (unless (org-hermes--buffer-http-success-p)
            (error "The org-hermes endpoint returned a non-success response"))
          t)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun org-hermes--local-follow-function (scheme)
  "Return the original local Org :follow function for SCHEME."
  (plist-get (if (equal scheme "http")
                 org-hermes--previous-http-parameters
               org-hermes--previous-https-parameters)
             :follow))

(defun org-hermes-open-url-locally (scheme path arg)
  "Open SCHEME:PATH locally using the previous Org behavior.
ARG is the prefix argument passed by Org."
  (let ((follow (org-hermes--local-follow-function scheme)))
    (if follow
        (funcall follow path arg)
      (browse-url (org-hermes--url scheme path) arg))))

(defun org-hermes-follow (scheme path arg)
  "Follow Org HTTP link PATH with SCHEME through org-hermes.
ARG is passed through to the local fallback opener."
  (let ((url (org-hermes--url scheme path)))
    (condition-case err
        (org-hermes-open-url-remotely url)
      (error
       (if org-hermes-fallback-to-local
           (progn
             (when org-hermes-notify-on-fallback
               (message "org-hermes remote open failed: %s; opening locally"
                        (error-message-string err)))
             (org-hermes-open-url-locally scheme path arg))
         (signal (car err) (cdr err)))))))

(defun org-hermes--http-follow (path arg)
  "Follow an Org http link PATH.
ARG is passed through to the local fallback opener."
  (org-hermes-follow "http" path arg))

(defun org-hermes--https-follow (path arg)
  "Follow an Org https link PATH.
ARG is passed through to the local fallback opener."
  (org-hermes-follow "https" path arg))

;;; config.el ends here
