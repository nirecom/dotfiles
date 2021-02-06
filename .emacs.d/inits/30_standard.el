;;; 30_standard.el --- Standard Definitions without packages
;;; Commentary:
;;; Code:
;; Define Keys
(define-key global-map [?¥] [?\\]) ;; (macOS) type backslash instead of yen sign. may not work...?

;; Theme
(load-theme 'misterioso t)
;; Japanese, UTF-8
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Do not display startup message
(setq inhibit-startup-message t)

;; Do not create backup files
;(setq make-backup-files nil)

;; Create backup files under specific folder
;; ref. https://emacs.tsutomuonoda.com/file-settings-that-emacs-automatically-creates/
(setq backup-directory-alist '((".*" . "~/.emacs_backup")))
(setq version-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 1)
(setq delete-old-versions t)

;; Create auto-save files, but do not create auto-save list file
(setq auto-save-file-name-transforms   '((".*" "~/tmp/" t)))
(setq auto-save-list-file-prefix nil)

;; Remove auto save file when quit
(setq delete-auto-save-files t)

;; Use editorconfig
;; ref. https://qiita.com/10sr/items/5e5d9519874ea3602d96
(editorconfig-mode 1)

;; Display CRLF codes
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; Prohibit multiple windows
;(setq ns-pop-up-frames nil)

;; Remove Menu Bar
(menu-bar-mode -1)

;; Remove Tool Bar
(tool-bar-mode -1)

;; Display Column
;(column-number-mode t)

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

;;; 30_standard.el ends here
