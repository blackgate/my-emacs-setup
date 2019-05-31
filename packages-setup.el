(require 'package)
(require 'cl)

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

(require 'clojure-mode)
(put-clojure-indent 'fn-traced :defn)

(require 'use-package)

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
  :init
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-font-lock-dynamically '(macro core function var))
  (add-hook 'cider-mode-hook #'eldoc-mode))  

(use-package web-mode
  :ensure t
  :init
  (load "~/.emacs.d/web-mode-setup"))

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode))

(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files))
  :init
  (helm-mode 1))

(use-package magit
  :ensure t
  :bind
  (("C-c m" . magit-status)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 1))

(use-package treemacs
  :ensure t
  :init
  (setq treemacs-width 30))

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

(treemacs)
