;;; init.el --- Personal Initialization file
;;; Commentary:
;;; Install required packages, set them up for use.
;;; code:
(scroll-bar-mode -1)

(setq slime-default-lisp 'sbcl)
;(setq inferior-lisp-program "sbcl")

(prelude-ensure-module-deps '(jade-mode multiple-cursors go-mode
                                        js2-mode solarized-theme
                                        sublime-themes))
(require 'multiple-cursors)
;; (global-unset-key (kbd "M-<down-mouse-1>"))
;; (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'js2-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(set-frame-font "Monaco-12")
(disable-theme 'zenburn)
(load-theme 'solarized-light)
(display-time)

;; Full screen emacs
(toggle-frame-fullscreen)
