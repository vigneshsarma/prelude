;;; See: https://www.reddit.com/r/emacs/comments/3icpo7/take_a_break_every_3_hours/
;;; https://gist.github.com/camdez/5a770825d25cf66542fc

(defvar breaktime-timer nil
  "Holds the running break timer (if any).")
(defvar breaktime-interval (* 3 60 60)
  "How often to take a break, in seconds.")

(defun breaktime--take-a-break ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*breaktime*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (animate-string "Time to take a break"
                    (/ (window-height) 2) (- (/ (window-width) 2) 12)))
  (set-buffer-modified-p nil)
  (view-mode))

(defun breaktime-start ()
  (interactive)
  (when breaktime-timer
    (cancel-timer breaktime-timer))
  (setq breaktime-timer
        (run-at-time t breaktime-interval 'breaktime--take-a-break)))

(defun breaktime-stop ()
  (interactive)
  (when breaktime-timer
    (cancel-timer breaktime-timer)
    (setq breaktime-timer nil)))
