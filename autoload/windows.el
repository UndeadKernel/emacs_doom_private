;;; private/boy/autoload/windows.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +boy--get-buffer-tree (wintree)
  "Taken from evil-window.el: Extracts the buffer tree from a given window-tree."
  (if (consp wintree)
      (cons (car wintree) (mapcar #'+boy--get-buffer-tree (cddr wintree)))
    (window-buffer wintree)))

;;;###autoload
(defun +boy--restore-window-tree (win tree)
  "Taken from evil-window.el: Restores the given buffer-tree layout as subwindows of win."
  (cond
   ((and (consp tree) (cddr tree))
    (let ((newwin (split-window win nil (not (car tree)))))
      (+boy--restore-window-tree win (cadr tree))
      (+boy--restore-window-tree newwin (cons (car tree) (cddr tree)))))
   ((consp tree)
    (set-window-buffer win (cadr tree)))
   (t
    (set-window-buffer win tree))))

;;;###autoload
(defun +boy--window-swap (direction)
  "Move current window to the next window in DIRECTION. If there are no windows
there and there is only one window, split in that direction and place this
window there. If there are no windows and this isn't the only window, use
+boy/window-move-* (e.g. `+boy/window-move-far-left')"
  (require 'windmove)
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (window-dedicated-p this-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
        (funcall (pcase direction
                   ('left  #'+boy/window-move-far-left)
                   ('right #'+boy/window-move-far-right)
                   ('up    #'+boy/window-move-very-top)
                   ('down  #'+boy/window-move-very-bottom)))
      (unless that-window
        (setq that-window
              (split-window this-window nil
                            (pcase direction
                              ('up 'above)
                              ('down 'below)
                              (_ direction))))
        (with-selected-window that-window
          (switch-to-buffer (doom-fallback-buffer)))
        (setq that-buffer (window-buffer that-window)))
      (with-selected-window this-window
        (switch-to-buffer that-buffer))
      (with-selected-window that-window
        (switch-to-buffer this-buffer))
      (select-window that-window))))

;;;###autoload
(defun +boy/window-move-left () "See `+boy--window-swap'"  (interactive) (+boy--window-swap 'left))
;;;###autoload
(defun +boy/window-move-right () "See `+boy--window-swap'" (interactive) (+boy--window-swap 'right))
;;;###autoload
(defun +boy/window-move-up () "See `+boy--window-swap'"    (interactive) (+boy--window-swap 'up))
;;;###autoload
(defun +boy/window-move-down () "See `+boy--window-swap'"  (interactive) (+boy--window-swap 'down))

;;;###autoload
(defun +boy/window-move-very-top ()
  "Taken from evil-window.el: Closes the current window, splits the upper-left one horizontally
and redisplays the current buffer there."
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (+boy--get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((newwin (selected-window))
              (subwin (split-window)))
          (+boy--restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

;;;###autoload
(defun +boy/window-move-far-left ()
  "Taken from evil-window.el: Closes the current window, splits the upper-left one vertically
and redisplays the current buffer there."
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (+boy--get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((newwin (selected-window))
              (subwin (split-window-horizontally)))
          (+boy--restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

;;;###autoload
(defun +boy/window-move-far-right ()
  "Taken from evil-window.el: Closes the current window, splits the lower-right one vertically
and redisplays the current buffer there."
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (+boy--get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((subwin (selected-window))
              (newwin (split-window-horizontally)))
          (+boy--restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

;;;###autoload
(defun +boy/window-move-very-bottom ()
  "Taken from evil-window.el: Closes the current window, splits the lower-right one horizontally
and redisplays the current buffer there."
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (+boy--get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((subwin (selected-window))
              (newwin (split-window)))
          (+boy--restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

;;;###autoload
(defun +boy/switch-to-last-window ()
  "Switch to the most recently used window."
  (interactive)
  (let ((win (get-mru-window nil t t)))
    (unless win (error "Most recent window not found."))
    (select-window win)))

;; https://lists.gnu.org/archive/html/auctex/2015-12/msg00010.html
;;;###autoload
(defun +boy/split-window-sensibly (&optional window)
  "Split vertically except if that would leave the average window horizontal
size smaller than 80."
  (let ((root (frame-root-window)))
    (cond
     ((>= (/ (window-total-width root)
             (1+ (window-combinations root t)))
          80)
      (split-window window nil 'right))
     ((and (< (window-combinations root) 2)
           (>= (/ (window-total-height root)
                  (1+ (window-combinations root)))
               20))
      (split-window window nil 'below))
     (t
      (let ((split-height-threshold nil))
        (split-window-sensibly window))))))
