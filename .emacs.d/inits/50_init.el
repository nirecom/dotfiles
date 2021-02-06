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

(setq whitespace-style '(face           ; faceで可視化
                            trailing       ; 行末
                            tabs           ; タブ
                            empty          ; 先頭/末尾の空行
                            spaces         ; 空白
                            ;; space-mark     ; 表示のマッピング
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
;    (global-set-key (kbd "M-x") 'counsel-M-x)
;    (global-set-key (kbd "M-y") 'counsel-yank-pop)
;    (global-set-key (kbd "C-M-z") 'counsel-fzf)
;    (global-set-key (kbd "C-M-r") 'counsel-recentf)
;    (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
;    (global-set-key (kbd "C-M-f") 'counsel-ag)
    ;; activate
    (counsel-mode 1))

;; swiper: Isearch with an overview. Oh, man!
(when (require 'swiper nil t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
;    (global-set-key (kbd "M-x") 'counsel-M-x) ; not necessary. pre-defined
;    (global-set-key (kbd "C-x C-f") 'counsel-find-file) ; not necessary. pre-defined
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    )

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
    (message "This platform is not mac")
)

;; Separate customized part to another file
;; ref. https://vinelinux.org/docs/vine6/emacs-guide/emacs-customize-saving-customizations.html
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p (expand-file-name custom-file))
        (load-file (expand-file-name custom-file)))
;;; 50_init.el ends here
