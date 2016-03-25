(load "~/.emacs.d/packages-setup")

(require 'use-package)
(require 'linum)

(setq custom-file "~/.emacs.d/custom.el")

(load custom-file)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq-default line-spacing 4)

(set-default-font "Input Mono 13")

(when window-system (set-frame-size (selected-frame) 120 40))

(setq inhibit-splash-screen t)

(menu-bar-mode t)

(setq org-src-fontify-natively t)

;(setq org-ditaa-jar-path "~/.emacs.d/vendor/ditaa0_9.jar")

;(org-babel-do-load-languages
; 'org-babel-load-languages
; '((ditaa . t)))

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(add-hook 'prog-mode-hook 'linum-mode)

(setq linum-format "%4d ")

(prefer-coding-system 'utf-8)

;; Packages

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package cider
  :ensure t
  :init
  (setq cider-repl-use-clojure-font-lock t))

(use-package web-mode
  :ensure t
  :init
  (load "~/.emacs.d/web-mode-setup"))

(use-package smex
  :ensure t
  :init
  (smex-initialize)
  :bind ("M-x" . smex))

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode));

(use-package atom-one-dark-theme
  :ensure t
  :init
  (load-theme 'atom-one-dark t)
  (set-face-attribute 'linum nil :foreground "#777"))
