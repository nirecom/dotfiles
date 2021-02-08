;;; 90_global-keys.el --- Keyboard Definitions
;;; Commentary:
;;; Code:
;; Define Keys
(define-key global-map [?¥] [?\\]) ;; (macOS) type backslash instead of yen sign. may not work...?
(global-set-key (kbd "<f12>") 'indent-region)
;;; 90_global-keys ends here
