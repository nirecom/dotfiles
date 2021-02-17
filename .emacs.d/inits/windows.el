;;; windows.el --- Windows-specific settings
;;; Commentary:
;;; Code:
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :family "Consolas" :height 110)
  (set-fontset-font 'nil 'japanese-jisx0208
                    (font-spec :family "メイリオ"))
  (add-to-list 'face-font-rescale-alist
               '(".*メイリオ.*" . 1.08))
    )
;;; windows.el ends here
