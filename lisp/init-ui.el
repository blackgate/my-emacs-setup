;;; init-package.el --- Init basic packages
;;; Commentary:
;;; Code:
(require 'package)
(require 'cl-lib)

(setq inhibit-splash-screen t)

(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

(global-hl-line-mode 1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq-default line-spacing 3)

(defun font-candidate (&rest font-list)
  "Return existing font which first match in the FONT-LIST."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) font-list))

(let ((f (font-candidate "SF Mono 13" "Fira Code 13" "Consolas 12" "Monospace 12")))
  (when f
    (add-to-list 'default-frame-alist `(font . ,f))
    (set-face-attribute 'default t :font f)))

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

(use-package all-the-icons
  :if window-system
  :ensure t
  :config
  (when (not (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t))
  (add-to-list
   'all-the-icons-dir-icon-alist
   '("google[ _-]drive" all-the-icons-alltheicon "google-drive" :height 0.9 :v-adjust -0.1)))

(when window-system (set-frame-size (selected-frame) 120 40))

(provide 'init-ui)
;;; init-ui.el ends here
