;;; lolo-common.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; Misc. programming modes
(use-package csv-mode)
(use-package csharp-mode)
(use-package cask-mode)
(use-package cmake-mode)
(use-package dart-mode)
(use-package julia-mode)
(use-package lua-mode)
(use-package mermaid-mode)
(use-package powershell)
(use-package scala-mode)
(use-package swift-mode)
(use-package v-mode)
(use-package vimrc-mode)
(use-package yaml-mode)

(use-package protobuf-mode
  :hook (protobuf-mode . (lambda ()
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

(provide 'lolo-common)
;;; lolo-common.el ends here
