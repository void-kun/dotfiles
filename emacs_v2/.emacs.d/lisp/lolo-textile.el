;;; lolo-textile.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(require-package 'textile-mode)

(setq auto-mode-alist
      (cons '("\\.textile\\'" . textile-mode) auto-mode-alist))

(provide 'lolo-textile)
;;; lolo-textile.el ends here
