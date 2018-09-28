;;; ~/.doom.d/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+boy/org-babel-hydra/body "~/.doom.d/autoload/org.el" nil t)
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

;; Hack to get Ipython to not move cursor to output window
;; Replace pop-to-buffer with display-buffer and move it
;;;###autoload
(defun +boy*ob-ipython--output (output append-p)
  (when (not (s-blank? output))
    (let ((buf (get-buffer-create "*ob-ipython-out*")))
      (save-excursion
        (display-buffer buf)
        (with-current-buffer buf
          (special-mode)
          (let ((inhibit-read-only t))
            (unless append-p (erase-buffer))
            (let ((p (point)))
              (if (= p (point-max))     ;allow tailing
                  (progn (insert output)
                         (-when-let (w (get-buffer-window buf 'visible))
                           (set-window-point w (point-max))))
                (save-excursion
                  (goto-char (point-max))
                  (insert output)))
              (ansi-color-apply-on-region p (point-max))
              ;; this adds some support for control chars
              (comint-carriage-motion p (point-max)))
            (unless append-p (goto-char (point-min)))))))))

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
         (org-hide-block-toggle))))))
