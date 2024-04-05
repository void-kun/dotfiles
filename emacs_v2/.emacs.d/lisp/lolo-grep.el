;;; lolo-grep.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(require-package 'wgrep)
(with-eval-after-load 'grep
  (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode)))

(when (and (executable-find "ag")
           (maybe-require-package 'ag))
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))

(when (and (executable-find "rg")
           (maybe-require-package 'rg))
  (global-set-key (kbd "M-?") 'rg-project))


(provide 'lolo-grep)
;;; lolo-grep.el ends here
