
;;; lolo-corfu.el --- xxx.	-*- lexical-binding: t -*-

;; Author: hoangzrik
;; URL: https://github.com/void-kun/dotfiles

;;; Code:


(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :load-path (lolo-vendor-dir "/corfu-terminal.el")
    :hook (global-corfu-mode . corfu-terminal-mode)))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))


(provide 'lolo-corfu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lolo-corfu.el ends here
