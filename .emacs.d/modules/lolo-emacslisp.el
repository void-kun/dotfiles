;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package eldoc
  :defer t
  :straight (:type built-in)
  :after company
  :init
  (eldoc-add-command 'company-complete-selection
                     'company-complete-common
                     'company-capf
                     'company-abort))

(use-package elisp-mode
  :straight (:type built-in)
  :requires smartparens
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (smartparens-mode -1))))

(use-package elisp-demos
  :defer t
  :straight (:build t)
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package epdh
  :straight (epdh :type git
                  :host github
                  :repo "alphapapa/emacs-package-dev-handbook"
                  :build t)
  :defer t)

(use-package package-lint
  :defer t
  :straight (:build t))

(use-package eask-api
 :defer t
 :straight (eask-api :type git
                     :host github
                     :repo "emacs-eask/eask-api"))

(use-package eask-mode
 :defer t
 :straight (eask-mode :type git
                      :host github
                      :repo "emacs-eask/eask-mode"))

;; ============================================================================
(use-package elisp-autofmt
    :defer t
    :straight (:build t))

;; ============================================================================
(use-package
 parinfer-rust-mode
 :defer t
 :straight (:build t)
 :diminish parinfer-rust-mode
 :hook emacs-lisp-mode common-lisp-mode scheme-mode
 :init
 (setq
  parinfer-rust-auto-download t
  parinfer-rust-library-directory (concat user-emacs-directory "parinfer-rust/"))
 (add-hook 'parinfer-rust-mode-hook (lambda () (smartparens-mode -1))))

(provide 'lolo-emacslisp)
;;; lolo-emacslisp.el ends here
