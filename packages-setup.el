(require 'package)
(require 'cl-lib)

(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
	("org"          . "http://orgmode.org/elpa/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 11)
	("org"          . 1)))

(package-initialize)

(defvar my-packages '(better-defaults
                      use-package
                      exec-path-from-shell
                      project-explorer))

(defun prelude-packages-installed-p ()
  (cl-loop for p in my-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))

(unless (prelude-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (unless (package-installed-p p)
      (package-install p))))

(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons)
  (all-the-icons-install-fonts t))

(require 'use-package)

(use-package clojure-mode
  :config
  (put-clojure-indent 'fn-traced :defn))

(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode))

(use-package all-the-icons
  :config
  (add-to-list
   'all-the-icons-dir-icon-alist
   '("google[ _-]drive" all-the-icons-alltheicon "google-drive" :height 0.9 :v-adjust -0.1)))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2               ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

(use-package cider
  :ensure t
  :config
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-global-mode)
  ;; FIXES PROJECTILE BUG
  (setq projectile-mode-line
	'(:eval (format " Projectile[%s]"
			(projectile-project-name)))))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package web-mode
  :ensure t
  :init
  (load "~/.emacs.d/web-mode-setup"))

(use-package magit
  :ensure t
  :bind
  (("C-c m" . magit-status)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 1)
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#758085"))))))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-width 30)
  (treemacs-git-mode 'simple))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-treemacs-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-github t))

(use-package org
  :ensure org-plus-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (clojure . t)
     (js . t)))
  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-src-preserve-indentation t
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t
	org-display-inline-images t
	org-redisplay-inline-images t
	org-startup-with-inline-images "inlineimages"
	org-startup-with-latex-preview t
	org-startup-indented t
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
	org-return-follows-link t
	org-confirm-babel-evaluate nil
	;; Don't indent things for nested headings (eg. properties)
	org-adapt-indentation nil
	;; Use UTF-8 ellipsis character
        org-ellipsis " ▼ "
	org-image-actual-width nil
	org-pretty-entities t
	org-hide-emphasis-markers t
	org-babel-clojure-backend 'cider)
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  (setq org-babel-js-function-wrapper "console.log(require('util').inspect(function(){\n%s\n}()));")
  (add-hook 'org-mode-hook #'auto-fill-mode))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(treemacs)
