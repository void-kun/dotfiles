;;; lolo-ui.el --------------------------------

;;; Code:
(when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(menu-bar-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; config font settings
(custom-set-faces
 '(default ((t (:family "IosevkaLyteTerm" :foundry "UKWN" :slant normal :weight regular :height 120 :width normal)))))

(set-face-attribute 'line-number nil :family "IosevkaLyteTerm" :height 143)
(set-face-attribute 'line-number-current-line nil :family "IosevkaLyteTerm" :height 143)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq display-line-numbers-type 'relative)
(setq linum-format "%d ")

;; show line numbers at the beginning of each line
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode)
    (global-nlinum-mode t))

;; enable y/n answers
(fset 'yes-nor-no-p 'y-or-n-p)

;; more useful frame title
(setq frame-title-format
    '("" invocation-name " Lolo - " (:eval (if (buffer-file-name)
                                        (abbreviate-file-name (buffer-file-name))
                                        "%b"))))

(when lolo-theme
    (load-theme lolo-theme t))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'which-key-mode)
    (which-key-mode +1))

(provide 'lolo-ui)
;;; lolo-ui.el ends here
