;;; +tools.el --- description -*- lexical-binding: t; -*-

;; LLM tools configuration
(after! gptel
  (setq! gptel-default-mode #'org-mode))

;; ssh-deploy config
(after! ssh-deploy
  (setq ssh-deploy-on-explicit-save nil))

;; pdf-tools config
;; enable visual-line-mode in the buffer where annotations
;; ... are shown (*Contents*)
(after! pdf-tools
  (defun boy/annot-visual-line (_id _buffer)
    (let ((contents-buf (get-buffer "*Contents*")))
      (when (and contents-buf (not visual-line-mode))
        (with-current-buffer contents-buf
          (visual-line-mode)))))
  ;; advice the function responsible for creating the *Contents* buffer
  (advice-add 'pdf-annot-list-context-function :after #'boy/annot-visual-line))
;; more fine-grained zooming
(setq pdf-view-resize-factor 1.1)
;; set some default properties for all annotations
(setq pdf-annot-default-annotation-properties
      '((t
         (label . "carlos")
         (color . "#483d8b"))
        (text
         (icon . "Note"))
        (highlight
         (color . "#eee8aa"))
        (squiggly
         (color . "orange"))
        (strike-out
         (color . "red"))))

;; Magit config
(add-hook! magit-mode (visual-line-mode +1))
;; search for Magit passwords in `auth-sources'
(add-hook 'magit-process-find-password-functions 'magit-process-password-auth-source)

;; EIN config
(setq +ein-notebook-dir "~")
(add-hook! ein:notebook-multilang-mode
  (map! :map ein:notebook-mode-map
        "M-p" #'+boy/up-scroll
        "M-n" #'+boy/down-scroll))

