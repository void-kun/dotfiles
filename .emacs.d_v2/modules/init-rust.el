;;; init-rust.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; (use-package
;;  rust-mode
;;  :mode "\\.rs\\'"
;;  :custom (rust-format-on-save t)
;;  :bind (:map rust-mode-map ("C-c C-c" . rust-run))
;;  :config
;;  (use-package
;;   flycheck-rust
;;   :after flycheck
;;   :config
;;   (with-eval-after-load 'rust-mode
;;     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

;; Rust
(use-package rustic)

(use-package ron-mode
  :mode ("\\.ron" . ron-mode))

(provide 'init-rust)
;;; init-rust.el ends here
