;;; init.el --- Personal Initialization file
;;; Commentary:
;;; Install required packages, set them up for use.
;;; code:
(scroll-bar-mode -1)
(prelude-ensure-module-deps
 '(stylus-mode multiple-cursors go-mode js2-mode jedi
               sphinx-doc ag rvm ido-vertical-mode tern tern-auto-complete
               yaml-mode anaconda-mode helm helm-projectile helm-ag
               hl-sexp tuareg virtualenvwrapper smart-mode-line rich-minority

               ;; themes
               noctilux-theme color-theme-sanityinc-tomorrow
               solarized-theme sublime-themes gotham-theme ujelly-theme
               arjen-grey-theme flatland-theme subatomic-theme

))

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
(load-theme 'subatomic)
;; (load-theme 'sanityinc-tomorrow-eighties)
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
(setq venv-location "/Users/vignesh/.venvs")

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

;; emacs from comandline
(x-focus-frame nil)

;; lisp
(setq slime-default-lisp 'sbcl)
(setq slime-contribs '(slime-fancy))
;(setq inferior-lisp-program "sbcl")

(require 'hl-sexp)
(require 'color)
(custom-set-faces
 `(hl-sexp-face ((t (:background ,(-> (face-attribute 'highlight :background)
                                      (color-lighten-name 4)))))))
(add-hook 'lisp-mode-hook 'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
(add-hook 'clojure-mode-hook 'hl-sexp-mode)

(require 'rich-minority)
(rich-minority-mode 1)
(setq rm-blacklist (mapconcat 'identity (list "ws" "guru" "company" "Pre") "\\|"))
