;;; init.el --- Personal Initialization file
;;; Commentary:
;;; Install required packages, set them up for use.
;;; code:
(scroll-bar-mode -1)

(setq slime-default-lisp 'sbcl)
(setq slime-contribs '(slime-fancy))
;(setq inferior-lisp-program "sbcl")

(prelude-ensure-module-deps '(stylus-mode multiple-cursors go-mode
                                          js2-mode solarized-theme
                                          sublime-themes jedi
                                          color-theme-sanityinc-tomorrow
                                          sphinx-doc ag rvm
                                          ido-vertical-mode
                                          tern tern-auto-complete
                                          tuareg virtualenvwrapper))

(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)
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
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(require 'stylus-mode)
(add-to-list 'auto-mode-alist '("\\.jade$" . stylus-mode))

(set-frame-font "Monaco-12")
(disable-theme 'zenburn)
;; (load-theme 'solarized-dark)
(load-theme 'sanityinc-tomorrow-eighties)
;; (display-time)

;; (add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)
                              (anaconda-mode t)))
(setq jedi:complete-on-dot t)
;; Full screen emacs
(toggle-frame-fullscreen)

(global-set-key (kbd "s-A") 'ag-project-files)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(setq ag-highlight-search t)

(setq flx-ido-threshold 1000)

(require 'ido-vertical-mode)
;; (ido-mode 1)
(ido-vertical-mode 1)

(require 's)
(require 'eshell)
(setq eshell-prompt-function
      (lambda ()
        (concat (if (s-blank? venv-current-name)
                    ""
                    (concat "(" venv-current-name ") "))
                (car (last (s-split "/" (eshell/pwd)))) " $ ")))

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "/Users/vigneshsarma/Envs")

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

(defun python-insert-encoding-at-point()
  (interactive)
  (insert "# -*- coding: utf-8 -*-"))

(define-key projectile-mode-map [?\s-f] 'projectile-find-file)

(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode nil)))

;; SaltStack
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))
