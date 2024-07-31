;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package lisp-mode
  :defer t
  :straight (:type built-in)
  :after parinfer-rust-mode
  :hook (lisp-mode . parinfer-rust-mode)
  :config
  (put 'defcommand 'lisp-indent-function 'defun)
  (setq inferior-lisp-program "/usr/bin/sbcl --noinform"))

(use-package stumpwm-mode
  :defer t
  :straight (:build t)
  :hook lisp-mode)

(use-package sly
  :defer t
  :straight (:build t))

(provide 'lolo-commonlisp)
;;; lolo-commonlisp.el ends here
