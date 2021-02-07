;;; package-install.el --- Installed listed packages
;;; Commentary:
;;; ref https://emacs-jp.github.io/packages/package
;;; Code:
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(package-refresh-contents)

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
    ;; company: completion
    company company-web
    ivy counsel swiper
    yasnippet yasnippet-snippets react-snippets
;    hydra; ivy-hydra
    ;; flycheck and related package
    flycheck add-node-modules-path
    ;; editorconfig
    editorconfig
    ;; several modes
    dockerfile-mode
    php-mode
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
;;; package-install.el ends here
