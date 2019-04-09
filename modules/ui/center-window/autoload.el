;;; ui/center-window/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +center-window/toggle-center-buffer ()
  (interactive)
  (if +center-window--window-conf
      (progn
        (set-window-configuration +center-window--window-conf)
        (setq +center-window--window-conf nil)
        ;; Delete our empty buffer
        (kill-buffer +center-window--empty-buffer))
    (setq +center-window--window-conf (current-window-configuration))
    ;; Maximize selected window
    (delete-other-windows)
    ;; Create a three-windows layout with our "centerd" window in the middle
    (let* ((width-percent (or +center-window-size-percent 0.4))
           (main-win-size (+center-window/percentage-to-width width-percent))
           (side-win-size (+center-window/percentage-to-width
                           (/ (- 1.0 width-percent) 2)))
           (empty-buffer (get-buffer-create +center-window--empty-buffer)))
      ;; Prepare the empty buffer
      (with-current-buffer empty-buffer
        (hide-mode-line-mode +1))
      ;; Prepare the layout
      (split-window-right side-win-size)
      (set-window-buffer nil empty-buffer)
      (other-window 1)
      (split-window-right main-win-size)
      (other-window 1)
      (set-window-buffer nil empty-buffer)
      ;; Move back to our "centered" window.
      (other-window -1))))

;;;###autoload
(defun +center-window/percentage-to-width (percentage &optional window)
  "Return a window width as an integer.
The width is the PERCENTAGE of WINDOW's frame's width."
  (round (* percentage (frame-width (window-frame window)))))
