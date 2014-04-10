;;; python-shift --- Toggle the selected region as commented or not.
;;; Commentary:
;;; Original idea from
;; https://github.com/gabrielelanaro/emacs-for-python/blob/master/epy-editing.el
;; patches by balle
;; http://www.datenterrorist.de

;;; Code:
(defun balle-python-shift-left ()
  "Shift python code in region left."
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
             mark-active)
        (setq start (region-beginning) end (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'line))
        (setq start (car bds) end (cdr bds))))
    (python-indent-shift-left start end))
  (setq deactivate-mark nil))

(defun balle-python-shift-right ()
  "Shift python code in region right."
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
             mark-active)
        (setq start (region-beginning) end (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'line))
        (setq start (car bds) end (cdr bds))))
    (python-indent-shift-right start end))
  (setq deactivate-mark nil))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-S-<tab>")
              'balle-python-shift-right)
            (define-key python-mode-map (kbd "C-<tab>")
              'balle-python-shift-left)))

;;; python-shift.el ends here
