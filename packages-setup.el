(require 'package)
(require 'cl)
(require 'linum)

(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(package-initialize)

(defvar my-packages '(better-defaults
                      use-package
                      exec-path-from-shell 
                      clojure-mode
                      project-explorer))

(defun prelude-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (unless (package-installed-p p)
      (package-install p))))

(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons)
  (all-the-icons-install-fonts t))

(require 'use-package)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package cider
  :ensure t
  :init
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-font-lock-dynamically '(macro core function var)))

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

(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t))

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

(use-package helm-dash
  :ensure t
  :config
  (setq helm-dash-browser-func 'eww))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 1)
  (set-face-foreground 'rainbow-delimiters-unmatched-face "red")
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#999999"))
