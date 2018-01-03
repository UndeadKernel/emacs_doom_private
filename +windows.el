;;; modules/private/boy/+windows.el -*- lexical-binding: t; -*-

(require 'windmove)

(defun +boy--get-buffer-tree (wintree)
  "Taken from evil-window.el: Extracts the buffer tree from a given window-tree."
  (if (consp wintree)
      (cons (car wintree) (mapcar #'+boy--get-buffer-tree (cddr wintree)))
    (window-buffer wintree)))

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

(defun +boy--window-swap (direction)
  "Move current window to the next window in DIRECTION. If there are no windows
there and there is only one window, split in that direction and place this
window there. If there are no windows and this isn't the only window, use
+boy/window-move-* (e.g. `+boy/-window-move-far-left')"
  (when (doom-popup-p)
    (doom/popup-raise))
  (let* ((this-window (get-buffer-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (doom-popup-p that-window))
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

