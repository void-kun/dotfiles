;;; lolo-haml.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(require-package 'haml-mode)

(with-eval-after-load 'haml-mode
  (define-key haml-mode-map (kbd "C-o") 'open-line))
  
(provide 'lolo-haml)
;;; lolo-haml.el ends here
