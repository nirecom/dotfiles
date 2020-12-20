;; configure load path
;(setq load-path (append
;		 '("~/.emacs.d"
;		   "~/.emacs.d/packages")))

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

;; Mac option key is meta key
;(setq mac-option-modifier 'meta)
			       
