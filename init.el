;;; init.el --- Init file
;;; Commentary:
;;; Code:
(setq gc-cons-threshold 100000000)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-package)
(require 'defaults-setup)
(require 'init-ui)
(require 'packages-setup)

(server-start)
;;; init.el ends here
