;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 go-mode
 :straight (:build t)
 :mode "\\.go\\'"
 :config (add-to-list 'exec-path "~/.go/bin"))

;; use gofumpt for format
(setq lsp-go-use-gofumpt t)

;; setup autosave for go-ts-mode
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-ts-mode-hook #'lsp-go-install-save-hooks)

(use-package go-tag :straight (:build t))

(provide 'lolo-go)
;;; lolo-go.el ends here
