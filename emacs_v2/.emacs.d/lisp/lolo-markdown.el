;;; lolo-markdown.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(when (maybe-require-package 'markdown-mode)
  (add-auto-mode 'markdown-mode "\\.md\\.html\\'")
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))

(provide 'lolo-markdown)
;;; lolo-markdown.el ends here
