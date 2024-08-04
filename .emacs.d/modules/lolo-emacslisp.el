;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:


;; ============================================================================
(use-package
 eldoc
 :defer t
 :after company
 :init
 (eldoc-add-command
  'company-complete-selection
  'company-complete-common
  'company-capf
  'company-abort))

(use-package package-lint :defer t)

;; ============================================================================
(use-package elisp-autofmt :defer t)

;; ============================================================================
(use-package
 parinfer-rust-mode
 :defer t
 :diminish parinfer-rust-mode
 :hook emacs-lisp-mode common-lisp-mode scheme-mode
 :init
 (setq
  parinfer-rust-auto-download t
  parinfer-rust-library-directory (concat user-emacs-directory "parinfer-rust/"))
 (add-hook 'parinfer-rust-mode-hook (lambda () (smartparens-mode -1))))

(use-package sly
  :defer t)

(provide 'lolo-emacslisp)
;;; lolo-emacslisp.el ends here
