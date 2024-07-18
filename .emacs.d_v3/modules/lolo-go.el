;;; lolo-go.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 go-mode
 :ensure t
 :mode "\\.go\\'"
 :config (add-to-list 'exec-path "~/.go/bin")
 :config (setq gofmt-command "goimports"))

(use-package go-tag :ensure t)

(use-package godoctor :ensure t)

(provide 'lolo-go)
;;; lolo-go.el ends here
