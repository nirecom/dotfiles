;; Initialize package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives  '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; configure load path
; adding default .emacs.d causes error
;(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/packages")

;; Use editorconfig (elisp version)
(setq editorconfig-get-properties-function
      'editorconfig-core-get-properties-hash)

;; Theme
(load-theme 'misterioso t)

;;
;; Highlighters
;;
;; Define general major modes
(require 'generic-x)

;; yaml-mode: highlighter
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; flymake-yaml for syntax check
;; ref. https://dev.classmethod.jp/articles/emacs-edit-yaml-cloudformation/
;(require 'flymake-yaml)
;(add-hook 'yaml-mode-hook 'flymake-yaml-load)

;; js-mode
(setq js-indent-level 2)

;; Markdown
;; ref. https://qiita.com/howking/items/bcc4e05bfb16777747fa
(package-install 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
;; Highlighten code block
(setq markdown-fontify-code-blocks-natively t)

;; Markdown Preview Mode
;; ref. https://qiita.com/No_217/items/afd39b0828b444a160da
(autoload 'markdown-preview-mode "markdown-preview-mode.el" t)

;; Japanese, UTF-8
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Do not display startup message
(setq inhibit-startup-message t)

;; Do not display backup file
(setq make-backup-files nil)

;; Remove auto save file when quit
(setq delete-auto-save-files t)

;; Use tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Display CRLF codes
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; Prohibit multiple windows
(setq ns-pop-up-frames nil)

;; Remove Menu Bar
(menu-bar-mode -1)

;; Remove Tool Bar
;(tool-bar-mode -1)

;; Display Column
(column-number-mode t)

; Display Line Number
;(global-linum-mode t)
; ref. https://qiita.com/mamo3gr/items/1c6862cba09d5876e52c
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode))
 
;; Show corresponding parenthesis
(show-paren-mode 1)

;; Visualize spaces and tabs
;(global-whitespace-mode 1)

;; Scroll one line only
(setq scroll-conservatively 1)

;; dired
;(require 'dired-x)

;; Disable ring
(setq visible-bell t)
;(setq ring-bell-function 'ignore)

;; Mac keybind
;(mac-key-mode 1)

;; ref. https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;(load custom-file)

;;
;; Auto added part by emacs
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode uuidgen solarized-theme markdown-preview-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
