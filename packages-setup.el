(require 'package)
(require 'cl)

(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

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
