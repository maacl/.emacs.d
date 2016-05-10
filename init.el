;; Package management
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(if (eq system-type 'darwin)
  (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el"))
(if (eq system-type 'gnu/linux)
  (require 'cask "~/.cask/cask.el"))

(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; Path
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; Constants
(defconst my/home (getenv "HOME"))
(defconst my/hostname system-name)
(defconst my/bin (concat my/home "/bin"))
(defconst my/emacs.d (concat my/home "/.emacs.d/"))

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-control-modifier 'control)
  (set-frame-font "Menlo-13"))

;; Add personal bin to path
(add-to-list 'exec-path my/bin)

;; Terminal specific key codes
(define-key input-decode-map "\e[1;10A" [M-S-up])
(define-key input-decode-map "\e[1;10B" [M-S-down])
(define-key input-decode-map "\e[1;10C" [M-S-right])
(define-key input-decode-map "\e[1;10D" [M-S-left])

;;Files
(setq
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 make-backup-files nil
 auto-save-default nil)

;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-style -1)
(setq inhibit-splash-screen t)
(setq visible-bell 1)
(blink-cursor-mode -1)
(global-hl-line-mode)
(require 'popwin)
(popwin-mode 1)

;; Themes
(defconst my/light-theme 'plan9)
(defconst my/dark-theme 'darktooth)

(defun my/change-theme (old new)
  (disable-theme old)
  (load-theme new t))

(defun my/dark ()
  (interactive)
  (my/change-theme my/light-theme my/dark-theme))

(defun my/light ()
  (interactive)
  (my/change-theme my/dark-theme my/light-theme))

(my/light)

(global-set-key (kbd "C-x d") 'my/dark)
(global-set-key (kbd "C-x l") 'my/light)

;; Commands
(require 'ido)
(ido-mode)
;;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")


(require 'ido-vertical-mode)
(ido-vertical-mode)

(setq ido-enable-flex-matching t)

(defalias 'yes-or-no-p 'y-or-n-p)

;;Editor
(setq make-backup-files nil)

(delete-selection-mode)

(setq whitespace-style '(face trailing tabs))

(add-hook 'prog-mode-hook 'whitespace-mode)

(setq tab-width 4)

(require 'ag)
(setq ag-reuse-buffers 't)

(global-set-key (kbd "C-c C-g") 'ag)

(require 'expand-region)
(global-set-key (kbd "M-e") 'er/expand-region)

;; Autocompletion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode 1)

;;ERC
(load "~/.ercpass")
(require 'erc)

(require 'erc-hl-nicks)
(erc-hl-nicks-enable)

(require 'erc-image)
(erc-image-enable)

(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)

(setq erc-nickserv-passwords
      `((freenode (("maacl" . ,freenode-maacl-pass)
		   ("MAACL" . ,freenode-maacl-pass)))))

(remove-hook 'erc-text-matched-hook 'erc-global-notify)

;;Programming
(add-hook 'prog-mode-hook 'show-paren-mode)

(setq show-paren-style 'expression)

(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Org
(require 'org)
;;(require 'ob-clojure)
(load-file "~/.emacs.d/ox-taskjuggler.el")
(add-to-list 'org-export-backends 'taskjuggler)

(setq org-return-follows-link t)
(setq org-babel-clojure-backend 'cider)

;; Clojure
(require 'cider)

(setq nrepl-hide-special-buffers t
      cider-popup-stacktraces nil
      cider-repl-popup-stacktraces t)

(setq cider-repl-result-prefix ";; => ")

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(require 'org-bullets)

(add-hook 'org-mode-hook 'org-bullets-mode)

;;SBCL
(load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl")


;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Babel

(defconst my/babel-languages '(emacs-lisp sh js haskell python clojure))

(org-babel-do-load-languages
  'org-babel-load-languages
   (mapcar (lambda (lang) (cons lang t)) my/babel-languages))

(setq org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)
;; Tasks

(defconst my/org-keywords
  '((sequence "TODO(t)" "INPROGRESS(i)" "WAIT(w)" "|" "DONE(d)")))

(setq org-todo-keywords my/org-keywords)

(setq org-enforce-todo-dependencies t)

(org-display-inline-images)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (geiser org pos-tip use-package smex smartparens slime rainbow-delimiters popwin plan9-theme pallet org-tree-slide org-bullets multiple-cursors markdown-mode magit ido-vertical-mode idle-highlight-mode flycheck fancy-narrow expand-region darktooth-theme company-quickhelp cider ag adoc-mode erc-hl-nicks erc-image))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
