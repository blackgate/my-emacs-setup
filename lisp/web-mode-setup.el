;;; web-mode-setup.el --- Web Mode setup
;;; Commentary:
;;; Code:
(require 'web-mode)

(setq web-mode-engines-alist '(("ctemplate" . "\\.html\\'")))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(provide 'web-mode-setup)

;;; web-mode-setup.el ends here
