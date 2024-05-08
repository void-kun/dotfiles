;;; lolo-rust.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'lolo-prog)

(use-package
  rustic
  :ensure
  :bind
  (:map
   rustic-mode-map
   ("M-j" . lsp-ui-imenu)
   ("M-?" . lsp-find-references)
   ("C-c C-c a" . lsp-execute-code-action)
   ("C-c C-c r" . lsp-rename)
   ("C-c C-c q" . lsp-workspace-restart)
   ("C-c C-c Q" . lsp-workspace-shutdown)
   ("C-c C-c s" . lsp-rust-analyzer-status)
   ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
   ("C-c C-c d" . dap-hydra)
   ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  (setq lsp-inlay-hint-enable t)
  (lsp-inlay-hints-mode)

  ;; comment to disable rustfmt on save
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(provide 'lolo-rust)
;;; lolo-rust.el ends here
