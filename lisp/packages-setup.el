;;; packages-setup.el --- Packages Setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (put-clojure-indent 'fn-traced :defn))

(use-package clj-refactor
  :ensure t
  :defer t
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
  :defines company-dabbrev-ignore-case company-dabbrev-downcase
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
  :defer t
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
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package xwidget
  :defer t
  :config
  ;; Fixes xwidget-webkit search
  (define-key xwidget-webkit-mode-map (kbd "C-s") 'isearch-forward))

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

(use-package js2-mode
  :ensure t
  :defer t
  :config
  ;; Flycheck provides these features, so disable them: conflicting with
  ;; the eslint settings.
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
	js2-idle-timer-delay 1.0)
  (setq js-switch-indent-offset 4))

(use-package web-mode
  :ensure t
  :defer t
  :init
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-engines-alist '(("ctemplate" . "\\.html\\'")))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode)))

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode ("\\.jsx?$" . rjsx-mode))

(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((web-mode . lsp)
	 (js-mode . lsp)
	 (js-jsx-mode . lsp))
  :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(rjsx-mode . "javascript")))

(use-package company-lsp
  :ensure t)

(use-package magit
  :ensure t
  :defer t
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
  :defer t
  :init
  (add-hook 'emacs-startup-hook 'treemacs)
  :config
  (advice-add 'treemacs-mode :after
              (lambda ()
                (interactive)
                (with-current-buffer (treemacs-get-local-buffer)
                  (setq-local face-remapping-alist
                              '((default . (:background "#20232A")))))))
  (setq treemacs-width 30)
  (treemacs-git-mode 'simple))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package restclient
  :ensure t
  :defer t
  :mode
  ("\\.http\\'" . restclient-mode))

(use-package company-restclient
  :ensure t
  :defer t
  :after (company restclient)
  :init
  (add-to-list 'company-backends 'company-restclient))

(use-package ob-http
  :ensure t
  :defer t
  :after (org restclient))

(use-package org-bullets
  :ensure t
  :defer t
  :after org
  :hook (org-mode . org-bullets-mode))

(defun org-mode-<>-syntax-fix (start end)
  "Change syntax of characters ?< and ?> to symbol within source code blocks from START to END."
  (let ((case-fold-search t))
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char start)
        (while (re-search-forward "<\\|>" end t)
          (when (save-excursion
                  (and
                   (re-search-backward "[[:space:]]*#\\+\\(begin\\|end\\)_src\\_>" nil t)
                   (string-equal (downcase (match-string 1)) "begin")))
            ;; This is a < or > in an org-src block
            (put-text-property (point) (1- (point))
                               'syntax-table (string-to-syntax "_"))))))))

(defun org-setup-<>-syntax-fix ()
  "Setup for characters ?< and ?> in source code blocks.  Add this function to `org-mode-hook'."
  (setq syntax-propertize-function 'org-mode-<>-syntax-fix)
  (syntax-propertize (point-max)))

(use-package org
  ;;:ensure org-plus-contrib
  :defer t
  :defines
  org-display-inline-images
  org-redisplay-inline-images
  org-hide-leading-stars-before-indent-mode
  org-babel-clojure-backend
  org-babel-js-function-wrapper
  :config
  ;;(require 'ox-extra)
  ;;(ox-extras-activate '(ignore-headlines))
  (require 'ob-clojure)
  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-src-preserve-indentation t
	org-src-window-setup 'current-window
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
	org-babel-clojure-backend 'cider
        org-babel-js-function-wrapper "console.log(require('util').inspect(function(){\n%s\n}()));")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (clojure . t)
     (js . t)
     (http . t)
     (scheme . t)
     (dot . t)))
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'org-setup-<>-syntax-fix)
  (set-face-background 'org-block-begin-line nil)
  (set-face-attribute 'org-block-begin-line nil :underline t :overline nil)
  (set-face-attribute 'org-block-end-line nil :underline nil :overline t)
  (set-face-attribute 'org-document-title nil :height 180)
  (set-face-attribute 'org-link nil :weight 'medium)
  (require 'org-protocol))

(use-package org-download
  :defer t
  :after org
  :config
  (if (and window-system (eq system-type 'darwin))
      (setq org-download-screenshot-method "screencapture -i %s"))
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package geiser
  :ensure t
  :defer t
  :defines geiser-default-implementation
  :hook (scheme-mode . geiser-mode)
  :config
  (setq geiser-default-implementation 'racket))

(use-package org-roam
  :ensure t
  :defer t
  :hook (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/roam/")
  :config
  (require 'org-roam-protocol)
  (require 'company-org-roam)
  (push 'company-org-roam company-backends)
  (setq org-roam-graph-viewer "/usr/bin/open")
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
- source :: ${ref}"
           :unnarrowed t)))
    (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  :bind
  (:map org-roam-mode-map
   (("C-c n l" . org-roam)
    ("C-c n f" . org-roam-find-file)
    ("C-c n j" . org-roam-jump-to-index)
    ("C-c n b" . org-roam-switch-to-buffer)
    ("C-c n g" . org-roam-graph))
   :map org-mode-map
   (("C-c n i" . org-roam-insert))))

(use-package org-roam-server
  :ensure t
  :defer t
  :config
  (setq org-roam-server-host "0.0.0.0"
        org-roam-server-port 8090
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package deft
  :ensure t
  :defer t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/roam/"))

(use-package ox-hugo
  :ensure t
  :defer t
  :after ox)

(use-package xwidget
  :defer t
  :config
  (defun xwidget-webkit-dark-mode ()
    "Turn dark mode on."
    (interactive)
    (xwidget-webkit-execute-script
     (xwidget-webkit-current-session)
     "document.body.style.cssText = 'filter: invert(1); background: #222;'")))

(provide 'packages-setup)
;;; packages-setup.el ends here
