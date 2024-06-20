;;; init-ui.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; config font settings
(custom-set-faces
 '(default ((t (:family "VictorMono Nerd Font Propo" :foundry "UKWN" :slant normal :weight regular :height 200 :width normal)))))

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(add-to-list 'default-frame-alist '(font . "ZedMono Nerd Font-20"))
;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when lolo-minimalistic-ui
  (menu-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; show line numbers at the beginning of each line
(unless lolo-minimalistic-ui
  ;; there's a built-in linum-mode, but we're using
  ;; display-line-numbers-mode or nlinum-mode,
  ;; as it's supposedly faster
  (if (fboundp 'global-display-line-numbers-mode)
      (global-display-line-numbers-mode)
    (global-nlinum-mode t)))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Lolo - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; use gruber-darker as the default theme
(when lolo-theme
  (load-theme lolo-theme t))

;; https://github.com/justbur/emacs-which-key/issues/306
(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'which-key-mode)
  (which-key-mode +1))

(provide 'init-ui)
;;; init-ui.el ends here
