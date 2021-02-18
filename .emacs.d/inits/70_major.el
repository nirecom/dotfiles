;;; 70_major.el --- Major Modes
;;; Commentary:
;;; Code:
(require 'use-package)

;; lsp-java: Emacs Java IDE using Eclipse JDT Language Server.
;; ref https://github.com/emacs-lsp/lsp-java
(use-package lsp-mode
    :ensure t
    :defer t
    :init
    (setq lsp-keymap-prefix "C-c l")
    (setq lsp-keep-workspace-alive nil
        lsp-signature-doc-lines 5
        lsp-idle-delay 0.5
        lsp-prefer-capf t
      lsp-client-packages nil)
    :config
    (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
    (setq lsp-completion-enable-additional-text-edit nil))

(use-package lsp-java
    :config
    (add-hook 'java-mode-hook 'lsp))

;(require 'lsp-java)
;(add-hook 'java-mode-hook #'lsp)

;(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

;; Org-Mode
;; ref https://www.mhatta.org/wp/2018/09/23/org-mode-101-6/#orged2066b
(use-package org
    :defer t ; don't read Org at launch
    :mode
    (("\\.txt$" . org-mode)) ; txt and md on Org mode additionally
    (("\\.md$" . org-mode)) ; https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:org:start
    :config
    (setq org-directory "~/Dropbox/Org"
        org-default-notes-file "notes.org"
        org-agenda-files '("~/Dropbox/Org/gtd.org"
                              "~/Dropbox/Org/notes.org")
        org-log-done 'time
        org-startup-truncated nil
        org-use-speed-commands t
        org-enforce-todo-dependencies t)
    (setq org-todo-keywords
        '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))
    (setq org-refile-targets
        (quote ((nil :maxlevel . 3)
                   (mhatta/org-buffer-files :maxlevel . 1)
                   (org-agenda-files :maxlevel . 3))))
    (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Dropbox/Org/gtd.org" "INBOX")
              "* TODO %?\n %i\n %a")
             ("j" "Journal" entry (function org-journal-find-location)
                 "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
             ("n" "Note" entry (file+headline "~/Dropbox/Org/notes.org" "Notes")
                 "* %?\nEntered on %U\n %i\n %a")
             ))
    ;; org-tempo
    (use-package org-tempo
        :straight nil)
    :bind
    (("\C-xa" . org-agenda)
        ("\C-xc" . org-capture)
        ("\C-xh" . org-store-link))
    )

(provide '70_major)
;;; 70_major.el ends here
