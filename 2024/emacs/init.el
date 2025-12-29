;;; init.el --- Emacs initialization file  -*- lexical-binding: t; -*-

;;; Commentary:
;; A refreshed emacs configuration with my most used packages and configurations.
;; Updated with the help of Claude.


;;; Package Management
(setq package-enable-at-startup nil)

;;;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	(or (bound-and-true-p straight-base-dir)
	    user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; Bootstrap use-package
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(straight-use-package 'bind-key)
(require 'use-package)
(require 'bind-key)

(use-package project
  :straight (:type built-in))

; (require 'package)
; (setq package-enable-at-startup nil)
; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
; (add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
; (add-to-list 'package-archives '("nongnu-elpa" . "https://elpa.nongnu.org/nongnu/"))
; (package-initialize)

; (unless (package-installed-p 'use-package)
;   (package-refresh-contents)
;   (package-install 'use-package))

; (setq use-package-always-ensure t)

; (eval-when-compile
;   (require 'use-package))
; (require 'bind-key)

;;; Customization
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; UI Configuration
;;;; Disable unnecessary UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (boundp 'fringe-mode)
  (fringe-mode -1))
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;;; Set margins
(setq-default left-margin-width 1 right-margin-width 1)

;;;; Mouse settings
(xterm-mouse-mode)
(setq mouse-wheel-progressive-speed nil
      focus-follows-mouse "auto-raise"
      mouse-autoselect-window t)

;;;; Theme management
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

;;;; Startup settings
(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore)

;;;; Window management
(winner-mode t)

;;;; Display settings
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq fci-rule-column 80)
(pixel-scroll-mode)
(setq compilation-window-height 15)
(setq-default truncate-lines t)

;;; Evil Mode
(use-package evil
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode t)
  (evil-ex-define-cmd "W[rite]" 'save-buffer)
  (evil-ex-define-cmd "V[split]" 'evil-window-vsplit))

(use-package evil-collection
  :config
  (evil-collection-init))

;;; Org Mode
(use-package org
  :config
  (setq org-src-window-setup 'other-window
  org-src-fontify-natively t
  org-src-tab-acts-natively t
  org-edit-src-content-indentation 0
  org-fontify-quote-and-verse-blocks t
  org-confirm-babel-evaluate nil
  org-hide-emphasis-markers t
  org-startup-with-inline-images t
  org-fast-tag-selection-single-key 'expert)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (ditaa . t)
     (python . t)
     (C . t)
     (shell . t)))

  (add-to-list 'org-src-lang-modes '("html" . web))

  (add-hook 'org-babel-after-execute-hook
      (lambda ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images))))

  (add-hook 'org-mode-hook 'auto-fill-mode)

  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)

  (defun org-font-lock-ensure ()
    (font-lock-fontify-buffer))

  (defun tag-at-point-in-heading ()
    "Returns the tag at the current point in the string"
    (let ((str (buffer-string))
    (begin (point))
    (end (point)))
      (while (not (equal (aref str begin) ?:))
  (setq begin (- begin 1)))
      (while (not (equal (aref str end) ?:))
  (setq end (+ end 1)))
      (substring str (+ 1 begin) end)))

  (defun open-sparse-view ()
    "Shows a sparse tree on clicking a tag instead of org-tags-view"
    (when (and (org-element-lineage (org-element-context)
      '(headline inlinetask)
      t)
   (progn (save-excursion (beginning-of-line)
        (looking-at org-complex-heading-regexp))
    (and (match-beginning 5)
   (> (point) (match-beginning 5)))))
      (org-match-sparse-tree nil (concat "+" (tag-at-point-in-heading)))
      t))

  (add-hook 'org-open-at-point-functions 'open-sparse-view))

;;; Tmux integration
(setq tr--last-command nil)

(defun tr (command)
  "Run the specified command in the currently active tmux pane"
  (interactive "sCommand: ")
  (setq tr--last-command command)
  (call-process "tmux" nil nil nil "send-keys" command "Enter"))

(defun trr ()
  "Re-run the previous command"
  (interactive)
  (if tr--last-command
      (call-process "tmux" nil nil nil "send-keys" tr--last-command "Enter")
    (message "No available previous command!")))

(defun trb ()
  (interactive)
  (call-process "tmux" nil nil nil "send-keys" (buffer-string) "Enter"))

(defun trl ()
  (interactive)
  (call-process "tmux" nil nil nil "send-keys" (thing-at-point 'line) "Enter"))

(defun trh (start end)
  (interactive "r")
  (call-process "tmux" nil nil nil "send-keys" (buffer-substring start end) "Enter"))

(global-set-key (kbd "C-c x") 'tr)
(global-set-key (kbd "C-c r") 'trr)
(global-set-key (kbd "C-c b") 'trb)
(global-set-key (kbd "C-c h") 'trh)
(global-set-key (kbd "C-c l") 'trl)

;;; Compilation
(define-key evil-normal-state-map (kbd "C-c c") 'recompile)

;;; Man Pages
(setq Man-notify-method 'pushy)

;;; Editing Configuration
(setq-default c-basic-offset 2
        tab-width 2
        indent-tabs-mode nil
        auto-save-default nil
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;; Make sure default is overridden
(setq-default indent-tabs-mode nil)

;;; For bash
(add-hook 'sh-mode-hook
       (lambda ()
         (setq indent-tabs-mode nil)))

;;;; Smartparens
(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (define-key smartparens-mode-map (kbd "M-f") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-b") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-s") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-k") 'sp-kill-sexp))

;;;; Parenthesis highlighting
(show-paren-mode t)

;;;; Whitespace management
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq require-final-newline t)

;;; Menus
(use-package ivy
  :config
  (ivy-mode 1))

(use-package counsel
  :config
  (counsel-mode 1))

;;; Language/Project Specific Configuration
;;;; BUCK files
(add-to-list 'auto-mode-alist '(".*/BUCK$" . python-mode))

;;;; Web Mode
(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2
  web-mode-css-indent-offset 2
  web-mode-code-indent-offset 2
  web-mode-style-padding 2
  web-mode-script-padding 2
  web-mode-auto-quote-style 2))

;;;; Monky (Mercurial)
(use-package monky
  :config
  (setq monky-process-type 'cmdserver)

  (defun hg-file-history ()
    (interactive)
    (require 'monky)
    (monky-run-hg-async
     "log"
     "--template"
     "\n{rev}) {date|shortdate}/{author|user}\n{desc|fill68}\n↘\n"
     buffer-file-name)))

;;; Utilities
(defun path ()
  "Display the full path of the current buffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

;;; GDB Configuration
(setq gdb-many-windows t)

;;; Dired Configuration
(add-hook 'dired-mode-hook
    (lambda ()
      (dired-hide-details-mode 1)))

(setq dired-use-ls-dired nil)

;;; Web Browsing
(setq w3m-use-cookies t)

;;; Auto Completion
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (add-hook 'company-mode-hook
      (lambda ()
  (define-key evil-insert-state-map (kbd "C-.") 'company-complete)))
  (setq company-tooltip-align-annotations t
  company-idle-delay 0.1
  company-minimum-prefix-length 2))

;;; Buffer Management
(defun close-all-buffers ()
  "Close all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun revert-all-buffers ()
  "Refresh all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (buffer-file-name)
  (revert-buffer t t t)))))

;;; Theme Manipulation
(defun desaturate-color (color-hex)
  "Converts a color string to its desaturated equivalent hex string"
  (require 'color)
  (apply
   'color-rgb-to-hex
   (append (apply
      'color-hsl-to-rgb
      (apply
       'color-desaturate-hsl
       `(,@(apply 'color-rgb-to-hsl (color-name-to-rgb color-hex)) 100)))
     '(2))))

(defun transform-theme-colors (fn)
  "Apply FN to the colors on every active face.

   FN should accept the face symbol and the current color,
   and return the new color to be applied."
  (interactive)
  (mapc
   (lambda (face)
     (mapc
      (lambda (attr)
  (let ((current (face-attribute face attr)))
    (unless (or (not current)
    (listp current)
    (string= current "unspecified")
    (string= current "t"))
      (set-face-attribute face nil attr (funcall fn face current)))))
      '(:foreground :background :underline :overline :box :strike-through
  :distant-foreground))
     (mapc
      (lambda (complex-attr)
  (let* ((full (copy-tree (face-attribute face complex-attr)))
   (current (if (listp full) (member :color full))))
    (unless (or (not current)
    (not (listp full)))
      (setcar (cdr current) (funcall fn face (cadr current)))
      (set-face-attribute face nil complex-attr full))))
      '(:underline :overline :box)))
   (face-list)))

(defun desaturate-theme ()
  "Desaturate all currently active face colors."
  (interactive)
  (transform-theme-colors
   (lambda (face color)
     (desaturate-color color))))

(defun invert-theme ()
  "Take the complement of all currently active colors."
  (interactive)
  (require 'color)
  (transform-theme-colors
   (lambda (face color)
     (apply
      'color-rgb-to-hex
      (color-complement color))))
  (let ((current-ns-appearance (assoc 'ns-appearance default-frame-alist)))
    (cond ((eq (cdr current-ns-appearance) 'light)
     (setf (cdr current-ns-appearance) 'dark))
    ((eq (cdr current-ns-appearance) 'dark)
     (setf (cdr current-ns-appearance) 'light)))))

;;; Mode Line Configuration
(setq mode-line-format
      (list
       "%& %b%n"
       " ~ "
       "%m"
       " ~ "
       "%l:%c"))

;;; Performance Tweaks
(setq-default xterm-query-timeout nil)

;;; JavaScript Configuration
(setq js-indent-level 2)

;;; Add any local files
(setq local-config
      (concat user-emacs-directory "local.el"))
(when (file-exists-p local-config)
  (load-file local-config))


;;; Markdown configuration
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  ;; Enable visual-line-mode (word wrap)
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'markdown-mode-hook 'variable-pitch-mode))

;;; Zig
(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'"
  :hook (zig-mode . eglot-ensure))

;;; Go
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '(go-mode . ("/home/knl/go/bin/gopls")))))


;;; C
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
   '(go-mode . ("/home/knl/go/bin/gopls"))
    ((c-mode c++-mode) . ("clangd" "-j=8" "--clang-tidy" "--enable-config"))))

(global-set-key (kbd "C-c o") 'ff-find-other-file)

;;; VSC mode configurations
(setq vc-follow-symlinks t)

;;; Bookmarks
(setq bookmark-save-flag 1)

(provide 'init)

;;; Python
(use-package python-mode
  :ensure t
  :hook (python-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))))
;;; init.el ends here
