;;; lolo-rust.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package rust-mode
  :ensure t)

(use-package rust-ts-mode
  :ensure t
  :after (eglot)
  :hook ((rust-ts-mode . eglot-ensure)
	   (rust-ts-mode . company-tng-mode)
	   (rust-ts-mode . (lambda ()
						 (eglot-inlay-hints-mode -1))))
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer"))))

  (provide 'lolo-rust)
  ;;; lolo-rust.el ends here
