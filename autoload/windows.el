;;; private/boy/autoload/windows.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +boy/window-move-left () "See `+boy--window-swap'"  (interactive) (+boy--window-swap 'left))
;;;###autoload
(defun +boy/window-move-right () "See `+boy--window-swap'" (interactive) (+boy--window-swap 'right))
;;;###autoload
(defun +boy/window-move-up () "See `+boy--window-swap'"    (interactive) (+boy--window-swap 'up))
;;;###autoload
(defun +boy/window-move-down () "See `+boy--window-swap'"  (interactive) (+boy--window-swap 'down))

;;;###autoload
(defun +boy/switch-to-last-window ()
  "Switch to the previously selected window, skipping any other window in between."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found."))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

