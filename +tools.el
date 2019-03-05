;;; +tools.el --- description -*- lexical-binding: t; -*-

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

;; Magit config
(add-hook! magit-mode (visual-line-mode +1))

;; EIN config
(setq +ein-notebook-dir "~/Documents/CASED/Development")
(add-hook! ein:notebook-multilang-mode
  (map! :map ein:notebook-mode-map
        "M-p" #'+boy/up-scroll
        "M-n" #'+boy/down-scroll))

