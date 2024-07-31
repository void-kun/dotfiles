;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 cc-mode
 :straight (:type built-in)
 :defer t
 :init
 (put 'c-c++-backend 'safe-local-variable 'symbolp)
 (add-hook 'c-mode-hook #'tree-sitter-hl-mode)
 (add-hook 'c++-mode-hook #'tree-sitter-hl-mode)
 :config (require 'compile))

(use-package
 clang-format+
 :straight (:build t)
 :defer t
 :init (add-hook 'c-mode-common-hook #'clang-format+-mode))

(use-package
 modern-cpp-font-lock
 :straight (:build t)
 :defer t
 :hook (c++-mode . modern-c++-font-lock-mode))

(provide 'lolo-c)
;;; lolo-c.el ends here
