;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/packages-setup")

(require 'use-package)
(require 'linum)
(require 'cl)

(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(setq custom-file "~/.emacs.d/custom.el")

(load custom-file)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(unless (eq system-type 'darwin)
  (cua-mode t)
  (global-set-key (kbd "C-f") 'isearch-forward)
  (global-set-key (kbd "C-s") 'save-buffer)
  (define-key isearch-mode-map "\C-f" 'isearch-repeat-forward))

(setq-default line-spacing 3)

(set-default-font (font-candidate '"SF Mono 14" "Consolas 12"))

(when window-system (set-frame-size (selected-frame) 120 40))

(setq inhibit-splash-screen t)

(menu-bar-mode t)

(setq org-src-fontify-natively t)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(prefer-coding-system 'utf-8)

(setq cursor-in-non-selected-windows nil)
(setq-default cursor-in-non-selected-windows nil)

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

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode))

(use-package atom-one-dark-theme
  :ensure t
  :init
  (load-theme 'atom-one-dark t)
  (set-face-attribute 'linum nil :foreground "#777"))

(use-package all-the-icons
  :config
  (add-to-list
   'all-the-icons-dir-icon-alist
   '("google[ _-]drive" all-the-icons-alltheicon "google-drive" :height 0.9 :v-adjust -0.1)))

(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'ascii))
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-show-updir-line nil)
  (setq neo-cwd-line-style 'button)
  :config
  (let ((faces '(neo-button-face
                 neo-dir-link-face
                 neo-file-link-face
                 neo-header-face
                 neo-expand-btn-face
                 neo-banner-face
                 neo-root-dir-face)))
    (dolist (f faces)
      (set-face-attribute f nil :font (font-candidate "Helvetica Neue 13" "Segoe UI 10")))))

(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)))

(use-package magit
  :ensure t
  :bind
  (("C-c m" . magit-status)))

(use-package nlinum
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (setq nlinum-format "%4d "))
