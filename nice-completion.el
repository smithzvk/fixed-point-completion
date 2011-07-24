
(defun row-in-window ()
  (interactive)
  (let ((row 0))
    (save-excursion
     (beginning-of-visual-line)
     (while (>= (point) (window-start) )
       (beginning-of-visual-line 0)
       (incf row) ))
    row ))

(defun row-in-frame ()
  (interactive)
  (destructuring-bind (left top right bottom) (window-edges)
    (+ top (row-in-window)) ))

(defun set-row-in-frame (row-in-frame)
  (let* ((window-start (window-start))
         (window-start-row window-start)
         (current-row (row-in-frame))
         (discrepancy (- row-in-frame current-row)) )
    (save-excursion
     (goto-char (window-start))
     (while (> discrepancy 0)
       (previous-line)
       (decf discrepancy) )
     (while (< discrepancy 0)
       (next-line)
       (incf discrepancy) )
     ;; We are now at the desired starting point
     (setf new-window-start (point)) )
    (set-window-start (selected-window) new-window-start) ))

;; We are going to try and make completions behave better
(add-to-list 'special-display-buffer-names '("*Completions*" my-display-completions))

(defun my-display-completions (buf)
  "If we find that there is already a visible *Completions*
window, use it, otherwise if there is only one window in this
fram, put a new completion window \(popups) in a small window at
the top of the current frame, otherwise, just let Emacs do what
it would have done without my intervention."
  (let* (new-window
         (current-window (selected-window))
         (frame-windows (window-list frame 'no-minibuffer))
         ;; build a list of currently visible frames.  If there is a
         ;; *Completions* window in one of these, just use that one.
         (windows (apply 'append
                         (mapcar (lambda (frame)
                                   (when (eql (framep frame) 'x)
                                     (window-list frame 'no-minibuffer) ))
                                 (visible-frame-list) )))
         ;; Find The *Completions* window if it is there.
         (comp-window
           (find "*Completions*" windows
                 :test 'equal
                 :key (lambda (window)
                        (buffer-name (window-buffer window)) ))))
    (cond (comp-window
           ;; We already have a *Completions* window.  Just use that.
           comp-window )
          ((and pop-up-windows (eq 1 (length frame-windows)))
           ;; We only have one window in this frame, so let's split it
           (select-window (car frame-windows))
           (let ((row-in-frame (row-in-frame)))
             ;; Are we going to split at the head or the foot?
             (cond ((eql current-window (minibuffer-window))
                    ;; We need to treate the minibuffer specially
                    (setf new-window (split-window-vertically -10))
                    (setf target-window new-window)
                    (select-window current-window) )
                   ((< row-in-frame (floor (frame-height) 2))
                    (setf new-window (split-window-vertically -10))
                    (setf target-window new-window)
                    (select-window current-window)
                    (set-row-in-frame row-in-frame) )
                   (t
                    (setf new-window (split-window-vertically 10))
                    (setf target-window current-window)
                    (select-window new-window)
                    (set-row-in-frame row-in-frame) ))
             (set-window-buffer target-window buf)
             target-window ))
          ;; Lastly, we just let emacs do what it wants to do
          (t (let ((special-display-buffer-names
                     (remove-if (lambda (x) (equal "*Completions*" (first x)))
                                special-display-buffer-names )))
               (display-buffer "*Completions*" t) )))))


