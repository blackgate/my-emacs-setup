(load "~/.emacs.d/packages-setup")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;(global-linum-mode t)

(setq-default line-spacing 4)

(set-default-font "Input Mono 13")

(when window-system (set-frame-size (selected-frame) 120 40))

(setq inhibit-splash-screen t)

(load-theme 'aurora t)

(setq cider-repl-use-clojure-font-lock t)

(menu-bar-mode t)

(add-hook 'after-init-hook 'global-company-mode)

(require 'racket-mode)

(defun racket-simply-scheme ()
  (interactive)
  (racket--repl-eval "(require (planet dyoo/simply-scheme:2:2))"))

(setq org-src-fontify-natively t)

;(require 'moe-theme)
;(moe-dark)

;(setq mac-control-modifier 'none)
;(setq ns-function-modifier 'control)

(load "~/.emacs.d/web-mode-setup")
(load "~/.emacs.d/rainbow-setup")
