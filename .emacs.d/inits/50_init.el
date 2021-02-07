;;; 50_init.el --- Misc definitions
;;; Commentary:
;;; Code:

;; auto-async-byte-compile
;; ref. https://www.yokoweb.net/2017/07/23/emacs-byte-compile/
(require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
(setq auto-async-byte-compile-suppress-warnings t)

;; git-gutter+
;; ref https://github.com/nonsequitur/git-gutter-plus
(global-git-gutter+-mode)

(global-set-key (kbd "C-x g") 'git-gutter+-mode) ; Turn on/off in the current buffer
  (global-set-key (kbd "C-x G") 'global-git-gutter+-mode) ; Turn on/off globally

  (eval-after-load 'git-gutter+
    '(progn
       ;;; Jump between hunks
       (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
       (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)

       ;;; Act on hunks
       (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
       (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
       ;; Stage hunk at point.
       ;; If region is active, stage all hunk lines within the region.
       (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
       (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
       (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
       (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
       (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)))

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
(require 'flymake-yaml)
(add-hook 'yaml-mode-hook 'flymake-yaml-load)

;;
;; web mode
;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
; must be web-mode for Flycheck

;; ref. https://qiita.com/kwappa/items/6bde1fe2bbeedc85023e
; open .js, jsx with web-mode
(add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
; open .js with .jsx edit mode
(setq web-mode-content-types-alist   ; do not to use defvar instead of setq
      '(("jsx" . "\\.js[x]?\\'")))
; comments
(add-hook 'web-mode-hook
  '(lambda ()
    (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))))

;;
;; Markdown
;;
;; ref. https://qiita.com/howking/items/bcc4e05bfb16777747fa
(require 'markdown-mode)
;(package-install 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
;; Highlighten code block
(setq markdown-fontify-code-blocks-natively t)

;; Markdown Preview Mode
;; ref. https://qiita.com/No_217/items/afd39b0828b444a160da
(autoload 'markdown-preview-mode "markdown-preview-mode.el" t)

;; Dockerfile Mode
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Display whitespaces and tabs
;; ref. https://yanqirenshi.hatenablog.com/entry/2016/07/03/Emacs%3A_whitespace_%E3%81%A7%E4%BD%99%E5%88%86%E3%81%AA%E7%A9%BA%E7%99%BD/%E3%82%BF%E3%83%96%E3%81%AB%E8%89%B2%E3%81%A5%E3%81%91
(require 'whitespace)

;; spaces
(set-face-foreground 'whitespace-space nil)
(set-face-background 'whitespace-space "gray33")
;; empty lines at top and bottom
(set-face-background 'whitespace-empty "gray33")
;; tabs
(set-face-foreground 'whitespace-tab nil)
(set-face-background 'whitespace-tab "gray33")
;;
(set-face-background 'whitespace-trailing "gray33")
(set-face-background 'whitespace-hspace "gray33")

(setq whitespace-style '(face           ; visualize
                            trailing       ; end of line
                            tabs
                            empty          ; empty lines at top / end of file
                            spaces
                            ; space-mark
                            tab-mark))

;; only zenkaku spaces are visualized
(setq whitespace-space-regexp "\\(\u3000+\\)")
;; change how to display tabs
(setq whitespace-display-mappings
    '((tab-mark ?\t [?\xBB ?\t])))
;; enable
(global-whitespace-mode 1)

;;
;; Completion Modes
;;
;; Company Mode
;; ref. http://company-mode.github.io/
(require 'company)
;(global-company-mode) ; this does not work. use after-init-hook
(add-hook 'after-init-hook 'global-company-mode)

;; ivy: completion package
;; ref https://qiita.com/takaxp/items/2fde2c119e419713342b
(when (require 'ivy nil t)
    ;; Assign M-o to ivy-hydra-read-action
    (when (require 'ivy-hydra nil t)
        (setq ivy-read-action-function #'ivy-hydra-read-action))
    ;; Include recent files and bookmarks to `ivy-switch-buffer' (C-x b) list
    (setq ivy-use-virtual-buffers t)
    ;; approve to issue command in mini buffer
    (when (setq enable-recursive-minibuffers t)
        (minibuffer-depth-indicate-mode 1)) ;; prompt depth
    ;; close mini buffer with mashing ESC
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    ;; wrap long prompt (including candidates)
    (setq ivy-truncate-lines nil)
    ;; wrap list when C-p
    (setq ivy-wrap t)
    ;; activate
    (ivy-mode 1))

;; counsel: Various completion functions using Ivy
(when (require 'counsel nil t)
    (setq counsel-find-file-ignore-regexp "\\.elc\\'")
    ;; activate
    (counsel-mode 1))

;; swiper: Isearch with an overview. Oh, man!
(when (require 'swiper nil t)
    ; Ivy-based interface to standard commands
    (global-set-key "\C-s" 'swiper)
;    (global-set-key (kbd "C-s") 'swiper-isearch)
;    (global-set-key (kbd "M-x") 'counsel-M-x) ; defined by default
;    (global-set-key (kbd "C-x C-f") 'counsel-find-file) ; defined by default
    (global-set-key (kbd "M-y") 'counsel-yank-pop)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "<f2> j") 'counsel-set-variable)
    (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
    (global-set-key (kbd "C-c v") 'ivy-push-view)
    (global-set-key (kbd "C-c V") 'ivy-pop-view)

    ; Ivy-based interface to shell and system tools
    (global-set-key (kbd "C-c c") 'counsel-compile)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c L") 'counsel-git-log)
    (global-set-key (kbd "C-c k") 'counsel-rg)
    (global-set-key (kbd "C-c m") 'counsel-linux-app)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-c J") 'counsel-file-jump)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (global-set-key (kbd "C-c w") 'counsel-wmctrl)

    ; Ivy-resume and other commands
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "C-c b") 'counsel-bookmark)
    (global-set-key (kbd "C-c d") 'counsel-descbinds)
;    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c o") 'counsel-outline)
    (global-set-key (kbd "C-c t") 'counsel-load-theme)
    (global-set-key (kbd "C-c F") 'counsel-org-file)

    (global-set-key (kbd "C-c a") 'counsel-ag)

    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
)

;; git-complete: Yet another completion engine powered by git grep
;; ref https://qiita.com/zk_phi/items/642b1e7dd12b44ea83ce
(require 'git-complete)
(global-set-key (kbd "C-c C-c") 'git-complete)

;;
;; Flycheck
;;
;; ref https://www.flycheck.org/en/latest/user/installation.html
(when (locate-library "flycheck")
  (require 'flycheck)
  (global-flycheck-mode)
)

;; ref https://www.m3tech.blog/entry/emacs-web-service#%E5%85%B1%E9%80%9A%E8%A8%AD%E5%AE%9A
(flycheck-add-mode 'javascript-eslint 'web-mode)
(eval-after-load 'web-mode
    '(add-hook 'web-mode-hook #'add-node-modules-path))

;; ref https://www.m3tech.blog/entry/emacs-web-service#%E5%85%B1%E9%80%9A%E8%A8%AD%E5%AE%9A
; auto run syntax check
(setq flycheck-check-syntax-automatically
    '(save idle-change mode-enabled))
(setq flycheck-idle-change-delay 1)

;; diminish
(require 'diminish)

(eval-after-load "company" '(diminish 'company-mode "comp"))
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "git-gutter+" '(diminish 'git-gutter+-mode "GitG"))
(diminish 'editorconfig-mode "EC")

; dumb-jump
; Jump to definition for 40+ languages without configuration
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
; ref https://qiita.com/blue0513/items/c0dc35a880170997c3f5
;(setq dumb-jump-mode t)
(setq dumb-jump-selector 'ivy) ;; leave selection to ivy
(setq dumb-jump-use-visible-window nil)
; M-. and M-, are overriden

; symbol-overlay: Highlight symbols with keymap-enabled overlays
(require 'symbol-overlay)
(add-hook 'prog-mode-hook #'symbol-overlay-mode)
(add-hook 'markdown-mode-hook #'symbol-overlay-mode)
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)
(global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)

;; Copy & Paste synchronization (macOS)
;; ref. https://hawksnowlog.blogspot.com/2017/04/clipboard-share-for-emacs.html
;; required to run: brew install reattach-to-user-namespace
(if (eq system-type 'darwin)
    (progn
      (defun copy-from-osx ()
    (shell-command-to-string "reattach-to-user-namespace pbpaste"))
      (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "reattach-to-user-namespace" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx)
    )
;    (message "This platform is not mac")
)

;;; 50_init.el ends here
