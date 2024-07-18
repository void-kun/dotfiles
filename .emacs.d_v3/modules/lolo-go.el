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
 :custom (go-ts-mode-indent-offset 4)
 :config (add-to-list 'exec-path "~/.go/bin"))

(use-package go-tag :ensure t)

(use-package godoctor :ensure t)

(provide 'lolo-go)
;;; lolo-go.el ends here
