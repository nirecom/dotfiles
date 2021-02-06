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

;;
;; Install packages if not exist
;;
(package-refresh-contents) ; it's too slow

;; TODO: solarized-theme actually isn't included in melpa list any more...?
(defvar my/favorite-packages
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
    ;; yaml
    yaml-mode flymake-easy flymake-yaml
    symbol-overlay
))

(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide '20_packages)
;;; 20_packages.el ends here
