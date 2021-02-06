;;; 20_package.el --- Initialize packages / Install packages if not exist
;;; Commentary:
;;; Code:

;;
;; Initialize packages
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives  '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;on top init.el now
;(package-initialize)

;; configure load path (other than .emacs.d)
;(add-to-list 'load-path "~/.emacs.d/packages")

;;
;; Install packages if not exist
;;
(require 'cl)

(defvar installing-package-list
    '(
    package-utils ; upgrade packages
    init-loader ; load separated init.el files
    auto-async-byte-compile ; automatically compiles el
    git-gutter+
    ;; completion
    company ; company mode
    ivy counsel swiper
    ;; flycheck and related package
    flycheck add-node-modules-path
    ;; editorconfig
    editorconfig
    ;; several modes
    dockerfile-mode
    php-mode
    smart-mode-line
    terraform-mode
    ;; markdown-preview-mode
    markdown-mode
    markdown-preview-mode websocket web-server uuidgen
    ;; kotlin-mode
    kotlin-mode flycheck-kotlin
    ;; Web / JSX
    web-mode rjsx-mode
    ;; yaml
    yaml-mode flymake-easy flymake-yaml
))

(let ((not-installed (loop for x in installing-package-list
                         when (not (package-installed-p x))
                         collect x)))
    (when not-installed
        (package-refresh-contents)
        (dolist (pkg not-installed)
            (package-install pkg))))

(provide '20_packages)
;;; 20_packages.el ends here
