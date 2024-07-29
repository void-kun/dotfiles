;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package lisp-mode
  :straight (:type built-in)
  :defer t
  :after parinfer-rust-mode
  :hook (lisp-mode . parinfer-rust-mode)
  :config
  (put 'defcommand 'lisp-indent-function 'defun)
  (setq inferior-lisp-program "/usr/bin/sbcl --noinform"))

(use-package stumpwm-mode
  :straight (:build t)
  :defer t
  :hook lisp-mode)

(use-package sly
  :defer t
  :straight (:build t))

(provide 'lolo-commonlisp)
;;; lolo-commonlisp.el ends here
