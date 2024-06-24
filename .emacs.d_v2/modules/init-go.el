;;; init-go.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 go-mode
 :mode "\\.go\\'"
 :hook (before-save . gofmt-before-save)
 :custom (gofmt-command "goimports"))

(provide 'init-go)
;;; init-go.el ends here
