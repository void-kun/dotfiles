;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(defun my/local-tab-indent ()
  (setq-local indent-tabs-mode 1))

(add-hook 'makefile-mode-hook #'my/local-tab-indent)

;; ============================================================================
(use-package pkgbuild-mode
  :defer t
  :custom
  (pkgbuild-update-sums-on-save nil)
  (pkgbuild-ask-about-save nil))

;; ============================================================================
(use-package toml-mode
  :defer t
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

(provide 'lolo-dsls)
;;; lolo-dsls.el ends here
