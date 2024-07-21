;;; lolo-rust.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; Rust
(use-package
 rust-mode
 :after eglot
 :mode ("\\.rs\\'" . rustic-mode)
 :init
 (setq
  rust-format-on-save t
  rust-mode-treesitter-derive t)
 :config

 (with-eval-after-load 'rust-mode
   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

 (define-derived-mode
  rustic-mode rust-mode "Rust"
  "Major mode for Rust code.

\\{rust-mode-map}")

 (setq auto-mode-alist
       (delete '("\\.rs\\'" . rust-mode) auto-mode-alist))
 (setq auto-mode-alist
       (delete '("\\.rs\\'" . rust-ts-mode) auto-mode-alist)))


(use-package ron-mode :mode ("\\.ron" . ron-mode))

(provide 'lolo-rust)
;;; lolo-rust.el ends here