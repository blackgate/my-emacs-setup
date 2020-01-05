(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

(unless (eq system-type 'darwin)
  (cua-mode t)
  (global-set-key (kbd "C-f") 'isearch-forward)
  (global-set-key (kbd "C-s") 'save-buffer)
  (define-key isearch-mode-map "\C-f" 'isearch-repeat-forward))

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

(setq-default line-spacing 3)

(let ((f (font-candidate "SF Mono 13" "Fira Code 13" "Consolas 12")))
  (add-to-list 'default-frame-alist `(font . ,f))
  (set-face-attribute 'default t :font f))
;;(set-default-font (font-candidate "SF Mono 13" "Fira Code 13" "Consolas 12"))

(setq inhibit-splash-screen t)

(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode 1)

(show-paren-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(when window-system (set-frame-size (selected-frame) 120 40))

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

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)
