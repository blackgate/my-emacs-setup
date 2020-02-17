;;; packages-setup.el --- Packages Setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package clojure-mode  :ensure t
  :config
  (put-clojure-indent 'fn-traced :defn))

(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all) ;; Reload snippets when adding/changing snippets
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  
  (defun my/company-to-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))

  (bind-key "<backtab>" 'my/company-to-yasnippet company-active-map)
  (bind-key "<backtab>" 'company-yasnippet)
  
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
  (projectile-mode 1))

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
  :init (global-flycheck-mode)
  :config (setq flycheck-emacs-lisp-load-path 'inherit))

;; ;; Emacs 27 only
;; (unless (version< emacs-version "27")
;;   (use-package js-mode
;;     :mode ("\\.jsx?$" . js-jsx-mode)))

(use-package js2-mode
  :ensure t
  :config
  ;; Flycheck provides these features, so disable them: conflicting with
  ;; the eslint settings.
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil)
  (setq js-switch-indent-offset 4))

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-engines-alist '(("ctemplate" . "\\.html\\'")))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode)))

(use-package rjsx-mode
  :ensure t
  :mode ("\\.jsx?$" . rjsx-mode))

(use-package tide
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook ((rjsx-mode . tide-setup)
         (rjsx-mode . tide-hl-identifier-mode))
  :config
  (flycheck-add-mode 'javascript-tide 'rjsx-mode))

;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((web-mode . lsp)
;; 	 (js-mode . lsp)
;; 	 (js-jsx-mode . lsp))
;;   :commands lsp
;;   :config
;;   (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascript")))

;; (use-package company-lsp
;;   :ensure t)

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
  (treemacs-git-mode 'simple)
  (add-hook 'emacs-startup-hook 'treemacs))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package restclient
  :ensure t
  :mode
  ("\\.http\\'" . restclient-mode))

(use-package company-restclient
  :ensure t
  :after (company restclient)
  :init
  (add-to-list 'company-backends 'company-restclient))

;; (use-package restclient-test
;;   :ensure t
;;   :hook
;;   (restclient-mode-hook . restclient-test-mode))

(use-package org
  ;;:ensure org-plus-contrib
  :config
  ;;(require 'ox-extra)
  ;;(ox-extras-activate '(ignore-headlines))
  (require 'ob-clojure)
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
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (clojure . t)
     (js . t)
     (http . t)))
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  (setq org-babel-js-function-wrapper "console.log(require('util').inspect(function(){\n%s\n}()));")
  (add-hook 'org-mode-hook #'auto-fill-mode))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

;; (use-package ob-restclient
;;   :ensure t
;;   :after org restclient
;;   :init
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((restclient . t))))

(use-package ob-http
  :ensure t
  :after (org restclient))

(provide 'packages-setup)
;;; packages-setup.el ends here
