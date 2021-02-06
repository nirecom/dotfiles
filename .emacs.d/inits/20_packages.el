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
;(package-initialize) ; on init.el

;; configure load path (other than .emacs.d)
;(add-to-list 'load-path "~/.emacs.d/packages")

;;
;; Install packages if not exist
;; ref https://qiita.com/catatsuy/items/5f1cd86e2522fd3384a0
;;
(require 'cl)

(defvar installing-package-list
    '(
    package-utils ; upgrade packages
    init-loader ; load separated init.el files
    auto-async-byte-compile ; automatically compiles el
    diminish
    dumb-jump ; Jump to definition for 40+ languages without configuration
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
;    smart-mode-line
    terraform-mode
    ;; markdown-preview-mode
    markdown-mode
    markdown-preview-mode websocket web-server uuidgen
    ;; kotlin-mode
    kotlin-mode flycheck-kotlin
    ;; Web / JSX
    web-mode
; rjsx-mode
    ;; yaml
    yaml-mode flymake-easy flymake-yaml
    symbol-overlay
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
