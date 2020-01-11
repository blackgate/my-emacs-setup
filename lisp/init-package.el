;;; init-package.el --- Init basic packages
;;; Commentary:
;;; Code:
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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (defvar my-packages
;;   '(use-package
;;     exec-path-from-shell))

;; (defun prelude-packages-installed-p ()
;;   (cl-loop
;;    for p in my-packages
;;    when (not (package-installed-p p)) do (cl-return nil)
;;    finally (cl-return t)))

;; (unless (prelude-packages-installed-p)
;;   (package-refresh-contents)
;;   (dolist (p my-packages)
;;     (unless (package-installed-p p)
;;       (package-install p))))

(provide 'init-package)

;;; init-package.el ends here
