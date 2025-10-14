;; -*- lexical-binding: t; -*-

;;; init.el --- Personal Initialization file
;;; Commentary:
;;; Install required packages, set them up for use.
;;; code:
(scroll-bar-mode -1)
;; (prelude-ensure-module-deps
;;  '(use-package jedi
;;     sphinx-doc ag rvm ido-vertical-mode
    ;; yaml-mode anaconda-mode helm helm-projectile
    ;; smart-mode-line rich-minority
    ;; restclient yasnippet
    ;;  irony

    ;; themes
    ;; material-theme
    ;; noctilux-theme color-theme-sanityinc-tomorrow
    ;; solarized-theme sublime-themes gotham-theme ujelly-theme
    ;; arjen-grey-theme flatland-theme subatomic-theme
    ;; twilight-bright-theme twilight-anti-bright-theme
    ;; darktooth-theme bubbleberry-theme ;; aurora-theme
    ;; dracula-theme
    ;; ))

(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)

(require 'use-package)

(setq use-package-always-ensure t)

(setq delete-selection-mode t)

  ;; aurora-theme
;; (use-package paradox)
;; (use-package arjen-grey-theme)
;; (use-package material-theme)
;; (use-package noctilux-theme)
;; (use-package  color-theme-sanityinc-tomorrow)
;; (use-package solarized-theme)
;; (use-package sublime-themes)
;; (use-package gotham-theme)
;; (use-package ujelly-theme)
;; (use-package dracula-theme)
;; (use-package bubbleberry-theme)
;; (use-package darktooth-theme)
;; (use-package twilight-anti-bright-theme)
;; (use-package twilight-bright-theme)
;; (use-package twilight-bright-theme)
;; (use-package subatomic-theme)
;; (use-package flatland-theme)

(use-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package prodigy
  :commands prodigy
  :config
  (load-file (expand-file-name "personal/prodigy-service-defs.el"
                               user-emacs-directory)))


(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

;; (set-face-attribute 'default nil :height 125 :family "Source Code Pro")
;; (set-frame-font "Firamono-12")
;; (set-frame-font "Hack-12")
;; (set-frame-font "Monaco-14")
;; (set-face-attribute 'default nil :height 142 :family "JetBrains Mono")
;; (set-face-attribute 'default nil :height 135 :family "Anonymous Pro")
;; (set-frame-font "Consolas-13")
;; (set-frame-font "Monaco-13")
;; (set-frame-font "Fira Code-13")
(set-frame-font "Cascadia Code-12")
;; (set-frame-font "Jetbrains Mono-13")

;; (set-frame-font "Inconsolata-14.5")
(disable-theme 'zenburn)
;; (load-theme 'arjen-grey)
;; (load-theme 'solarized-light)
;; (load-theme 'material)
;; (load-theme 'solarized-dark)
;; (load-theme 'sanityinc-tomorrow-eighties)
;; (display-time)
;; (disable-theme 'arjen-grey)

;; (use-package apropospriate-theme
;;   :ensure t
;;   :config
;;   ;; (load-theme 'apropospriate-light t)
;;   ;; or
;;   (load-theme 'apropospriate-dark t))

(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t))

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

(use-package helm-projectile
  :config
  (setq helm-projectile-ignore-strategy 'search-tool)
  (global-set-key (kbd "s-A") 'helm-projectile-rg))

(global-set-key (kbd "C-c C-SPC") 'avy-goto-char)

(setq flx-ido-threshold 1000)

;; (require 'ido-vertical-mode)
;; ;; (ido-mode 1)
;; (ido-vertical-mode 1)

(require 's)
(require 'eshell)
(setq eshell-prompt-function
      (lambda ()
        (concat ;; (if (s-blank? venv-current-name)
                ;;     ""
                ;;     (concat "(" venv-current-name ") "))
                (car (last (s-split "/" (eshell/pwd)))) " $ ")))

;; (require 'virtualenvwrapper)
;; (venv-initialize-interactive-shells) ;; if you want interactive shell support
;; (venv-initialize-eshell) ;; if you want eshell support
;; (setq venv-location "/Users/vigneshS/.venvs")

(setq ivy-initial-inputs-alist nil) ;; removes ^, which makes the search regex

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

(defun python-insert-encoding-at-point ()
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
;; (load (expand-file-name "~/.roswell/helper.el"))
;; (setq inferior-lisp-program "ros -Q run")
;; (setq slime-contribs '(slime-fancy))
;; (setf slime-lisp-implementations
;;       `((sbcl    ("sbcl" "--dynamic-space-size" "2000"))
;;         (roswell ("ros" "-Q" "run"))))
;; (setf slime-default-lisp 'roswell)
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

(setq clojure-indent-style :always-align)
(with-eval-after-load 'clojure-mode
  (define-clojure-indent
   (or 0)
   (and 0)
   (:require 0)
   (:import 0)

   (facts 1)
   (fact 1)
   (mlet 1)
   (state! 1)
   (with-redefs 1)
   (with-latency-log 2)
   (has-entries 1)
   (future-via 1)))

;; (require 'rich-minority)
;; (rich-minority-mode 1)
;; (setq rm-blacklist (mapconcat 'identity (list "ws" ;; whitespace-mode
;;                                               "guru" ;; warn when using arrow keys and such
;;                                               "company" ;; autocomplete
;;                                               "Pre" ;; prelude mode
;;                                               "ARev"
;;                                               "hl-sexp"
;;                                               "EditorConfig"
;;                                               "ivy"
;;   ;;; auto-revert-mode, unmodifide file which are modified outside of emacs gets autoreloaded.
;;                                               ) "\\|"))

(setq-default c-basic-offset 2
              tab-width 2
              indent-tabs-mode 'f)
;; Org Mode

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode)
            (whitespace-mode -1)))
(use-package ob-http)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)
                               (http . t)))

;; Paradox
(setq paradox-github-token t) ;;"dd4cf72ae0215d68438f42a811ebffe025f6f9df"

(setq-default indent-tabs-mode nil)

;; svn find file projectile overide.
(custom-set-variables '(projectile-svn-command "find . -type f -print0"))

(setq lsp-enable-snippet nil)

(setq checkdoc-package-keywords-flag nil)
(setq checkdoc-arguments-in-order-flag nil)

(add-hook 'gfm-mode-hook (lambda () (whitespace-mode -1)))

;; scroll one line at a time (less "jumpy" than defaults)

;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 3))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq cider-prompt-for-symbol nil)
(setq nrepl-log-messages t)
(setq cider-stacktrace-print-length 30)
(setq cider-stacktrace-print-level 50)

;; "ag --line-numbers -S --hidden --color --nogroup %s %s %s"
;; (setq helm-grep-ag-command "rg -i --no-heading --line-number %s %s %s")

;; (use-package pipenv
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended))

(use-package web-mode
  :defer t
  :mode ("\\.phtml\\'"
         "\\.html\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'" ))

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package smex)

;; (setq max-lisp-eval-depth 400) ;;
;; (toggle-debug-on-error)

(use-package wsd-mode)

(setq org-startup-with-inline-images t)

(use-package ledger-mode
  :config
  (setq ledger-post-auto-adjust-amounts t))

(setq magit-save-repository-buffers 'dontask)

(use-package dhall-mode
  :defer t
  :config
  (setq dhall-type-check-inactivity-timeout 60))

(use-package nov)

(use-package jwt
  :defer t)

(setq inferior-lisp-program "ros run")

(use-package compile-angel
  :ensure t
  :demand t
  :config
  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  ;; (setq compile-angel-verbose t)

  ;; Uncomment the line below to compile automatically when an Elisp file is saved
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files before they are loaded
  ;; using `load' or `require'.
  (compile-angel-on-load-mode 1))
