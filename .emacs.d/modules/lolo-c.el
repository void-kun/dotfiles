
;;; lolo-c.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'lolo-custom))

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
         ("<f12>" . compile))
  :init (setq-default c-basic-offset 4))

(when (lolo-treesit-available-p)
  (use-package c-ts-mode
    :init (setq c-ts-mode-indent-offset 4)))

(provide 'lolo-c)
;;; lolo-c.el ends here
