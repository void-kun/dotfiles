;;; lolo-snippet.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; Yet another snippet extension
(use-package
 yasnippet
 :diminish yas-minor-mode
 :hook (after-init . yas-global-mode))

;; Collection of yasnippet snippets
(use-package yasnippet-snippets)

;; Yasnippet Completion At Point Function
(use-package
 yasnippet-capf
 :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(provide 'lolo-snippet)
;;; lolo-snippet.el ends here
