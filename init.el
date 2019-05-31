;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; FIXES COMPANY-BOX BUG WHEN IN FULLSCREEN
;;(setq ns-use-native-fullscreen nil)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; handle emacs utf-8 input
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")

(defun isolate-kill-ring()
  "Isolate Emacs kill ring from OS X system pasteboard.
This function is only necessary in window system."
  (interactive)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))

(defun pasteboard-copy()
  "Copy region to OS X system pasteboard."
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end) "pbcopy"))

(defun pasteboard-paste()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (shell-command-on-region
   (point) (if mark-active (mark) (point)) "pbpaste" nil t))

(defun pasteboard-cut()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (pasteboard-copy)
  (delete-region (region-beginning) (region-end)))

;; macos only
(if (and window-system (eq system-type 'darwin))
    (progn
      (isolate-kill-ring)
      ;; bind CMD+C to pasteboard-copy
      (global-set-key (kbd "s-c") 'pasteboard-copy)
      ;; bind CMD+V to pasteboard-paste
      (global-set-key (kbd "s-v") 'pasteboard-paste)
      ;; bind CMD+X to pasteboard-cut
      (global-set-key (kbd "s-x") 'pasteboard-cut)))

(setq mouse-drag-copy-region nil)
(delete-selection-mode)

(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(load "~/.emacs.d/packages-setup.el")
(load "~/.emacs.d/defaults-setup.el")
