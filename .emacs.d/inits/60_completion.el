;;; 60_completion.el --- Completions
;;; Commentary:
;;; Code:
(require 'use-package)

;; Company Mode
;; ref http://company-mode.github.io/
;(require 'company)
;(global-company-mode) ; this does not work. use after-init-hook
(add-hook 'after-init-hook 'global-company-mode)

;; ref https://qiita.com/kod314/items/3a31719db27a166d2ec1
(with-eval-after-load 'company
    (setq company-auto-expand t) ;; automatically expand 1 candidate
    (setq company-transformers '(company-sort-by-backend-importance)) ;; sort order
    (setq company-idle-delay 0)  ;; complete after x sec
    (setq company-minimum-prefix-length 3) ; 4 by default
    (setq company-selection-wrap-around t)
;    (setq completion-ignore-case t)
    (setq company-dabbrev-downcase nil)
    (global-set-key (kbd "C-M-i") 'company-complete)

    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "C-h") nil) ; undefine
;    (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer)

    ; like auto-complete
    ; ref https://qiita.com/syohex/items/8d21d7422f14e9b53b17
    (set-face-attribute 'company-tooltip nil
        :foreground "black" :background "lightgrey")
    (set-face-attribute 'company-tooltip-common nil
        :foreground "black" :background "lightgrey")
    (set-face-attribute 'company-tooltip-common-selection nil
        :foreground "white" :background "steelblue")
    (set-face-attribute 'company-tooltip-selection nil
        :foreground "black" :background "steelblue")
    (set-face-attribute 'company-preview-common nil
        :background nil :foreground "lightgrey" :underline t)
    (set-face-attribute 'company-scrollbar-fg nil
        :background "orange")
    (set-face-attribute 'company-scrollbar-bg nil
                            :background "gray40")
)

;; diminish: shorten minor mode expression
(eval-after-load "company" '(diminish 'company-mode "Comp"))

;;
;; Company plugins
;;
(require 'company-web-html)
(add-to-list 'company-backends 'company-web-html)
;(define-key web-mode-map (kbd "C-c w") 'company-web-html)

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

; do not display minor mode
(eval-after-load "counsel" '(diminish 'counsel-mode))

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

;;
;; Yasnippet
;;
(use-package yasnippet
    :ensure t
    :config
    (yas-global-mode 1))
;(require 'yasnippet)
;(yas-global-mode 1)
; also requires yasnippet-snippets

; TAB for indent and complete key
; ref https://www.ncaq.net/2018/03/28/13/12/03/
(global-set-key (kbd "<tab>") 'company-indent-or-complete-common)

; display yasnippet as candidates of company
; ref https://github.com/keicy/.emacs.d/issues/75
(defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
        (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(defun set-yas-as-company-backend ()
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
    )
(add-hook 'company-mode-hook 'set-yas-as-company-backend)

; eacl: Emacs auto complete lines
; ref https://github.com/redguardtoo/eacl
(require 'eacl)
(global-set-key (kbd "C-c c") 'eacl-complete-line)
(global-set-key (kbd "C-c C-c") 'eacl-complete-multiline)
(with-eval-after-load 'grep
    (dolist (v '("node_modules"
                    ".sass_cache"
                    ".cache"
                    ".npm"
                    "build"))
        (add-to-list 'grep-find-ignored-directories v))
    (dolist (v '("*.elc"
                    "*.min.js"
                    "*.bundle.js"
                    "*.min.css"
                    "*.json"
                    "*.log"
                    "*.svg"
                    "README.md"))
            (add-to-list 'grep-find-ignored-files v)))

;; git-complete: Yet another completion engine powered by git grep
;; ref https://qiita.com/zk_phi/items/642b1e7dd12b44ea83ce
;(require 'git-complete)
;(global-set-key (kbd "C-c C-c") 'git-complete)

; smartparen: Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
; ref https://qiita.com/ShingoFukuyama/items/ed1af137a98e0028e025
(smartparens-global-mode)
(show-smartparens-global-mode) ; seems heavy
;(ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice)
;(ad-activate 'delete-backward-char)

;; lsp-java: Emacs Java IDE using Eclipse JDT Language Server.
;; ref https://github.com/emacs-lsp/lsp-java
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration))))
  :init
  (setq lsp-keep-workspace-alive nil
        lsp-signature-doc-lines 5
        lsp-idle-delay 0.5
        lsp-prefer-capf t
        lsp-client-packages nil)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-completion-enable-additional-text-edit nil))

(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

;(require 'lsp-java)
;(add-hook 'java-mode-hook #'lsp)

;(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

;;; 60_completion.el ends here
