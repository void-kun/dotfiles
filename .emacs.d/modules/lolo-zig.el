;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package zig-mode
  :defer t
  :after flycheck
  :hook (zig-mode . lsp-deferred)
  :config
  ;; This is from DoomEmacs
  (flycheck-define-checker zig
    "A zig syntax checker using the zig-fmt interpreter."
    :command ("zig" "fmt" (eval (buffer-file-name)))
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
    :modes zig-mode)
  (add-to-list 'flycheck-checkers 'zig))

(provide 'lolo-zig)
;;; lolo-zig.el ends here
