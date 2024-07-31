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

(use-package go-tag :straight (:build t))

(provide 'lolo-go)
;;; lolo-go.el ends here
