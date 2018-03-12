;; Package management
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package validate
  :ensure t)

;; Path
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Constants
(defconst my/home (getenv "HOME"))
(defconst my/bin (concat my/home "/bin"))

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-control-modifier 'control)
  (set-frame-font "Menlo-13"))

;; Fonts
(defun my-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("fn" . 955) ; λ
          )))

(global-prettify-symbols-mode 1)

;; Add personal bin to path
(add-to-list 'exec-path my/bin)

;; Terminal specific key codes
(define-key input-decode-map "\e[1;10A" [M-S-up])
(define-key input-decode-map "\e[1;10B" [M-S-down])
(define-key input-decode-map "\e[1;10C" [M-S-right])
(define-key input-decode-map "\e[1;10D" [M-S-left])

;;Files
;; disable lockfiles
;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html
(setq create-lockfiles nil)

;; store all backup files in the tmp dir
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Backup-Names.html
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;; store all autosave files in the tmp dir
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Files.html
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-style -1)
(setq inhibit-splash-screen t)
(setq visible-bell 1)
(blink-cursor-mode -1)

(use-package powerline
  :ensure t
  :init
  (powerline-default-theme)
  :config
  (setq powerline-default-separator 'utf-8))

;; chords
(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1))

(defun jc/switch-to-previous-buffer ()
  "Switch to previously open buffer. Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(key-chord-define-global "JJ" 'jc/switch-to-previous-buffer)

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

(use-package smex
  :ensure t)

;; Ivy
(use-package ivy
  :ensure t
  :bind
  ("C-s" . swiper)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c g" . counsel-git)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-c C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; Avy
(use-package avy
  :ensure t
  :chords (("jj" . avy-goto-char-2)
           ("jl" . avy-goto-line))
  :bind ("M-s" . avy-goto-char))

;; General
(defalias 'yes-or-no-p 'y-or-n-p)

;;Editor
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :bind ("C-z" . undo) ("C-S-z" . redo))

(use-package multiple-cursors
  :ensure t
  :bind (("M-." . mc/mark-next-like-this)
         ("M-," . mc/unmark-next-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(delete-selection-mode)

(setq tab-width 4)

(use-package ag
  :ensure t
  :commands ag
  :bind ("C-c C-g" . ag)
  :config
  (setq ag-reuse-buffers 't))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind ("M-e" . er/expand-region))

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind  ("C-x o" . ace-window))

;;Fancy Narrow
(use-package fancy-narrow
  :ensure t
  :config
  (fancy-narrow-mode)
  :bind ("C-x , ," . fancy-narrow-to-region)
        ("C-x , ." . fancy-widen))

;; Autocompletion
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode 1)))

;;Flycheck
(use-package flycheck
  :ensure t)

;; Guidance
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

;;ERC
(use-package erc
  :ensure t
  :init
  (load "~/.ercpass")
  :config
  (use-package erc-hl-nicks
    :ensure t
    :config
    (erc-hl-nicks-enable))
  (use-package erc-image
    :ensure t
    :config
    (erc-image-enable))
  (use-package erc-services
    :config
    (erc-services-mode 1))
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords
	`((freenode (("maacl" . ,freenode-maacl-pass)
		     ("MAACL" . ,freenode-maacl-pass)))))

  (remove-hook 'erc-text-matched-hook 'erc-global-notify))

;;Programming
(add-hook 'prog-mode-hook 'show-paren-mode)
(setq show-paren-style 'mixed)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; org-mode
(use-package org
  :ensure t
  :init
  (bind-key "C-c a" 'org-agenda)
  :config
  (setq org-agenda-files '("~/Dropbox/documents/work/GFK/timecard.org"))
  (setq org-return-follows-link t)
  (setq org-babel-clojure-backend 'cider)

  ;; org-mode babel

  (defconst my/babel-languages '(emacs-lisp js haskell python clojure))

  (org-babel-do-load-languages
   'org-babel-load-languages
   (mapcar (lambda (lang) (cons lang t)) my/babel-languages))

  (setq org-src-tab-acts-natively t
	org-src-fontify-natively t
	org-confirm-babel-evaluate nil)

  ;; org-mode tasks
  (defconst my/org-keywords
    '((sequence "TODO(t)" "INPROGRESS(i)" "WAIT(w)" "|" "DONE(d)")))
  (setq org-todo-keywords my/org-keywords)
  (setq org-enforce-todo-dependencies t)

  ;; org-mode images
  (org-display-inline-images)

  ;; org-mode bullets
  (use-package org-bullets
    :ensure t
    :commands (org-bullets-mode)
    :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package org-tree-slide
    :ensure t)
  (use-package org-clock-csv
    :ensure t))

;;Markdown/AsciiDoc
(use-package markdown-mode
  :ensure t)
(use-package adoc-mode
  :ensure t)

;; Clojure
(use-package cider
  :ensure t
  :config
  (setq nrepl-hide-special-buffers t
	cider-popup-stacktraces nil
	cider-repl-popup-stacktraces t)
  (setq cider-repl-result-prefix ";; => ")
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'my-pretty-lambda)
  (add-hook 'clojurescript-mode-hook 'my-pretty-lambda)
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"))

(use-package clj-refactor               ; Refactoring utilities
  :ensure t
  :defer t
  :init
  (defun mu-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)                ; For adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c RET"))

  (add-hook 'clojure-mode-hook #'mu-clojure-mode-hook)
  :config
  (validate-setq cljr-suppress-middleware-warnings t
                 cljr-auto-sort-ns t
                 cljr-favor-prefix-notation
                 cljr-favor-private-functions)
  :diminish clj-refactor-mode)

;;HTML
(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t))

;; F#
(use-package fsharp-mode
  :ensure t
  :init
  (setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
  (setq fsharp-compiler "/usr/bin/fsharpc"))

;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  (progn
    (use-package intero
      :ensure t
      :config
      (progn
        (add-hook 'haskell-mode-hook 'intero-mode)))))

;;Racket
(use-package geiser
  :ensure t)

;;Lisp / SBCL
(use-package slime
  :ensure t)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; Git
;; Magit
(use-package magit
  :ensure
  :bind ("C-x g" . magit-status))
;; git-gutter
(use-package git-gutter
  :ensure
  :init (global-git-gutter-mode t))
;; git-timemachine
(use-package git-timemachine
  :ensure)
 
;; Themes
(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-high-contrast-mode-line t)

  (defcustom default-light-color-theme 'solarized-light
  "default light theme")

  (defcustom default-dark-color-theme 'solarized-dark
    "default dark theme")

  (defun toggle-dark-light-theme ()
    (interactive)

    (let ((is-light (find default-light-color-theme custom-enabled-themes)))
      (dolist (theme custom-enabled-themes)
	(disable-theme theme))
      (load-theme (if is-light default-dark-color-theme default-light-color-theme) t)))

  (if window-system
      (progn
	(global-set-key (kbd "<f11>") 'toggle-dark-light-theme)
	(load-theme 'solarized-light t))))
