;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(load "~/.emacs.d/packages-setup.el")
(load "~/.emacs.d/defaults-setup.el")

