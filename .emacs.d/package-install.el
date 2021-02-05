;; Installed listed
;; ref. https://emacs-jp.github.io/packages/package

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
    company ; company mode
    flycheck
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
    web-mode rjsx-mode
    ;; yaml
    yaml-mode flymake-easy flymake-yaml
))

(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))
