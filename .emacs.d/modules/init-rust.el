;;; init-rust.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(lolo/require-packages '(rust-mode
                         cargo
                         flycheck-rust
                         tree-sitter
                         tree-sitter-langs
                         yasnippet
                         ron-mode))

(require 'tree-sitter)
(require 'tree-sitter-langs)

(add-to-list 'super-save-predicates
             (lambda () (not (eq major-mode 'rust-mode))))

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

  ;; enable lsp for rust, by default it uses rust-analyzer as lsp server
  (add-hook 'rust-mode-hook 'lsp)

  ;; enable tree-sitter for nicer syntax highlighting
  (add-hook 'rust-mode-hook #'tree-sitter-mode)
  (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)

  (defun lolo/rust-mode-defaults ()
    ;; format on save
    (setq rust-format-on-save t)

    ;; lsp settings
    (setq
     ;; enable macro expansion
     lsp-rust-analyzer-proc-macro-enable t
     lsp-rust-analyzer-experimental-proc-attr-macros t)

    ;; Prevent #! from chmodding rust files to be executable
    (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

    ;; snippets are required for correct lsp autocompletions
    (yas-minor-mode)

    ;; CamelCase aware editing operations
    (subword-mode +1))

  (setq lolo-rust-mode-hook 'lolo/rust-mode-defaults)

  (add-hook 'rust-mode-hook (lambda ()
                              (run-hooks 'lolo-rust-mode-hook))))

(provide 'init-rust)
;;; init-rust.el ends here
