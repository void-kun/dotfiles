;;; lolo-rust.el --------------------------------

(require 'lolo-programming)

(lolo-require-packages '(rust-mode
                         cargo
                         flycheck-rust
                         tree-sitter
                         tree-sitter-langs
                         yasnippet
                         ron-mode))

(require 'tree-sitter)
(require 'tree-sitter-langs)

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

  (add-hook 'rust-mode-hook 'lsp)

  (add-hook 'rust-mode-hook #'tree-sitter-mode)
  (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)

  (defun lolo-rust-mode-defaults ()
    ;; format on save
    (setq rust-format-on-save t)

    ;; lsp settings
    (setq
     ;; enable macro expansion
     lsp-rust-analyzer-proc-macro-enable t
     lsp-rust-analyzer-experimental-proc-attr-macros t)

    (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

    (yas-minor-mode)

    (subword-mode +1))

  (setq lolo-rust-mode-hook 'lolo-rust-mode-defaults)

  (add-hook 'rust-mode-hook (lambda () (run-hooks 'lolo-rust-mode-hook))))

(provide 'lolo-rust)
;;; lolo-rust.el ends here
