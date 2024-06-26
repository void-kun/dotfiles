;;; init-rust.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; Rust
(use-package rustic)

(use-package ron-mode
  :mode ("\\.ron" . ron-mode))

(provide 'init-rust)
;;; init-rust.el ends here
