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
               restclient yasnippet
               ;; paradox
               elm-mode wsd-mode irony

               ;; themes
               noctilux-theme color-theme-sanityinc-tomorrow
               solarized-theme sublime-themes gotham-theme ujelly-theme
               arjen-grey-theme flatland-theme subatomic-theme
               twilight-bright-theme twilight-anti-bright-theme
               darktooth-theme bubbleberry-theme aurora-theme
               dracula-theme
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
;; (add-hook 'js-mode-hook (lambda () (tern-mode t)))
;; (eval-after-load 'tern
;;   '(progn
;;      (require 'tern-auto-complete)
;;      (tern-ac-setup)))

(require 'stylus-mode)
(add-to-list 'auto-mode-alist '("\\.jade$" . stylus-mode))

;; (set-face-attribute 'default nil :height 125 :family "Source Code Pro")
;; (set-frame-font "Fira-mono-12")
;; (set-frame-font "Hack-12")
(set-frame-font "Menlo-12")
;; (set-face-attribute 'default nil :height 135 :family "Anonymous Pro")
;; (set-frame-font "Consolas-13")
;; (set-frame-font "Monaco-12")
;; (set-frame-font "Inconsolata-14.5")
(disable-theme 'zenburn)
(load-theme 'arjen-grey)
;; (load-theme 'solarized-dark)
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

;; emacs from comandline
(x-focus-frame nil)
(setq ns-pop-up-frames nil)

(global-set-key (kbd "s-A") 'ag-project-files)
(global-set-key (kbd "C-c SPC") 'avy-goto-char)
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
(setq venv-location "/Users/vigneshS/.venvs")

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

;; lisp
(load (expand-file-name "~/.roswell/helper.el"))
;; (setq inferior-lisp-program "ros -Q run")
(setq slime-contribs '(slime-fancy))
(setf slime-lisp-implementations
      `((sbcl    ("sbcl" "--dynamic-space-size" "2000"))
        (roswell ("ros" "-Q" "run"))))
(setf slime-default-lisp 'roswell)
;;(setq inferior-lisp-program "sbcl")

;; for hl-sexp
(require 'hl-sexp)
(require 'color)
(custom-set-faces
 `(hl-sexp-face ((t (:background ,(-> (face-attribute 'highlight :background)
                                      (color-darken-name 4)))))))

;; setup paren-mode to hilight expressions
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)

(add-hook 'lisp-mode-hook 'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
(add-hook 'clojure-mode-hook 'hl-sexp-mode)
(setq clojure-defun-style-default-indent t)

(setq cider-test-infer-test-ns (lambda (ns)
                                 (let ((test-ns (format "%s-tests" ns)))
                                  (message test-ns)
                                  test-ns)))
(setq cider-test-show-report-on-success t)

(setq clojure-indent-style :always-indent)
(eval-after-load "clojure-mode"
  '(progn
     (define-clojure-indent
       (:require 0)
       (:import 0))))

(require 'rich-minority)
(rich-minority-mode 1)
(setq rm-blacklist (mapconcat 'identity (list "ws" ;; whitespace-mode
                                              "guru" ;; warn when using arrow keys and such
                                              "company" ;; autocomplete
                                              "Pre" ;; prelude mode
                                              "ARev"
  ;;; auto-revert-mode, unmodifide file which are modified outside of emacs gets autoreloaded.
                                              ) "\\|"))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.asp$" . web-mode))
(setq-default c-basic-offset 2
              tab-width 2
              indent-tabs-mode 'f)
;; Org Mode

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode)
            (whitespace-mode -1)))

(setq-default indent-tabs-mode nil)

;; svn find file projectile overide.
(custom-set-variables '(projectile-svn-command "find . -type f -print0"))
;; scroll one line at a time (less "jumpy" than defaults)

;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 3))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time
