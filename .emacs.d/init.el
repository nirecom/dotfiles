;;; init.el --- Initial file read by emacs
;;; Commentary:
;;; definitions are under inits folder controlled by init-loader.el.
;;; Code:

(package-initialize) ; must be top before init-loader-load
(init-loader-load)

;; Separate customized part to another file
;; ref. https://vinelinux.org/docs/vine6/emacs-guide/emacs-customize-saving-customizations.html
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p (expand-file-name custom-file))
    (load-file (expand-file-name custom-file)))

;;; init.el ends here
