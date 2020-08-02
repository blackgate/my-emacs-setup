;;; init-package.el --- Init basic packages
;;; Commentary:
;;; Code:
(require 'package)
(require 'cl-lib)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
	("org"          . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 11)
	("org"          . 1)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(provide 'init-package)

;;; init-package.el ends here
