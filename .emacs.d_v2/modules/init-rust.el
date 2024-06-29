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

(with-eval-after-load 'eglot
  ;; config rust
  (add-to-list
   'eglot-server-programs
   `(rust-mode
     .
     ("rust-analyzer"
      :initializationOptions
      (:procMacro
       (:enable t)
       :cargo (:buildScripts (:enable t) :features "all")))))
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (defun eglot-format-buffer-before-save-rust ()
    (add-hook 'before-save-hook #'rust-format-buffer))
  (add-hook 'rust-mode-hook #'defun eglot-format-buffer-before-save-rust))

(provide 'init-rust)
;;; init-rust.el ends here
