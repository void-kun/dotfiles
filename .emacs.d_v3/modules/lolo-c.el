;;; lolo-c.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; C/C++ Mode
(use-package
  cc-mode
  :ensure nil
  :bind (:map c-mode-base-map ("<f12>" . compile))
  :init (setq-default c-basic-offset 4))

(provide 'lolo-c)
;;; lolo-c.el ends here
