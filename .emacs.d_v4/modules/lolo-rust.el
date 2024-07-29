;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package rustic
  :defer t
  :straight (:build t)
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode-local-vars . rustic-setup-lsp)
  :hook (rustic-mode . lsp-deferred)
  :init
  (with-eval-after-load 'org
    (defalias 'org-babel-execute:rust #'org-babel-execute:rustic)
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))
  (setq rustic-lsp-client 'lsp-mode)
  (add-hook 'rustic-mode-hook #'tree-sitter-hl-mode)
  :config
  (setq rustic-indent-method-chain    t
        rustic-babel-format-src-block nil
        rustic-format-trigger         nil)
  (remove-hook 'rustic-mode-hook #'flycheck-mode)
  (remove-hook 'rustic-mode-hook #'flymake-mode-off)
  (remove-hook 'rustic-mode-hook #'rustic-setup-lsp))

(provide 'lolo-rust)
;;; lolo-rust.el ends here
