;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 go-mode
 :ensure t
 :mode "\\.go\\'"
 :config (add-to-list 'exec-path "~/.go/bin"))

(use-package go-tag :ensure t)

(provide 'lolo-go)
;;; lolo-go.el ends here
