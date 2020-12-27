;; Installed listed packages in my/favorite-packages
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
    solarized-theme
    ))

(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))
