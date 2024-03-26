;;; lolo-rust.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package rust-mode
  :defer t
  :init
  (setq rust-mode-treesitter-derive t)
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'lsp-deferred)
  (add-hook 'rust-mode-hook 
                (lambda () (prettify-symbols-mode)))
  )
        
(provide 'lolo-rust)
;;; lolo-rust.el ends here
