;;; init.el --- Personal Initialization file
;;; Commentary:
;;; Install required packages, set them up for use.
;;; code:
(scroll-bar-mode -1)
(prelude-ensure-module-deps
 '(use-package jedi
    sphinx-doc ag rvm ido-vertical-mode
    yaml-mode anaconda-mode helm helm-projectile helm-ag helm-rg
    hl-sexp virtualenvwrapper smart-mode-line rich-minority
    restclient yasnippet
    paradox irony ranger

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

(setq use-package-always-ensure t)

(use-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package prodigy
  :commands prodigy
  :ensure t
  :config
  (load-file (expand-file-name "personal/prodigy-service-defs.el"
                               user-emacs-directory)))


;; (set-face-attribute 'default nil :height 125 :family "Source Code Pro")
;; (set-frame-font "Fira-mono-12")
;; (set-frame-font "Hack-12")
(set-frame-font "Monaco-14")
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
(global-set-key (kbd "C-c C-SPC") 'avy-goto-char)
(setq ag-highlight-search t)

(setq flx-ido-threshold 1000)

;; (require 'ido-vertical-mode)
;; ;; (ido-mode 1)
;; (ido-vertical-mode 1)

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

;; Paradox
(setq paradox-github-token t) ;;"dd4cf72ae0215d68438f42a811ebffe025f6f9df"

(setq-default indent-tabs-mode nil)

;; svn find file projectile overide.
(custom-set-variables '(projectile-svn-command "find . -type f -print0"))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(setq racer-cmd "/Users/vigneshS/.cargo/bin/racer")
(setq racer-rust-src-path "/Users/vigneshS/code/rust/src/")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook (lambda ()
                            (define-key rust-mode-map (kbd "C-c C-c") 'recompile)))
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)

(setq checkdoc-package-keywords-flag nil)
(setq checkdoc-arguments-in-order-flag nil)


(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;; scroll one line at a time (less "jumpy" than defaults)

;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 3))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

;; clj refactor

;; (require 'clj-refactor)

;; (defun my-clojure-mode-hook ()
;;   (clj-refactor-mode 1)
;;   ;; (yas-minor-mode 1) ; for adding require/use/import statements
;;   ;; This choice of keybinding leaves cider-macroexpand-1 unbound
;;   (cljr-add-keybindings-with-prefix "C-c C-m"))

;; (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(setq cider-prompt-for-symbol nil)

;; "ag --line-numbers -S --hidden --color --nogroup %s %s %s"
;; (setq helm-grep-ag-command "rg -i --no-heading --line-number %s %s %s")

;; (setq sql-port 3306) ;; default MySQL port
