;;; 20_package.el --- Initialize packages / Install packages if not exist
;;; Commentary:
;;; Code:

;;
;; Initialize packages
;;
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
;(package-initialize) ; on init.el

;; configure load path (other than .emacs.d)
;(add-to-list 'load-path "~/.emacs.d/packages")

(provide '20_packages)
;;; 20_packages.el ends here
