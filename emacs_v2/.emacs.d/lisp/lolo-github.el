;;; lolo-github.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(require 'lolo-git)

(maybe-require-package 'yagist)
(require-package 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(maybe-require-package 'github-clone)
(maybe-require-package 'forge)
(maybe-require-package 'github-review)

(when (maybe-require-package 'flymake-actionlint)
  (add-hook 'yaml-mode-hook 'flymake-actionlint-action-load-when-actions-file))

(provide 'lolo-github)
;;; lolo-github.el ends here
