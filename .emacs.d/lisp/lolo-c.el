
;;; lolo-c.el --- xxx.	-*- lexical-binding: t -*-

;; Author: hoangzrik
;; URL: https://github.com/void-kun/dotfiles

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lolo-c.el ends here
