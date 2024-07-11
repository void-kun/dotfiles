;;; init-lsp.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 eglot
 :commands eglot
 :init (setq eglot-stay-out-of '(flymake))
 :custom
 (eglot-ignored-server-capabilites '(:documentHighlightProvider))
 (eglot-autoshutdown t)
 :hook (eglot-managed-mode . eldoc-box-hover-mode))

(with-eval-after-load 'eglot
  (load-library "project"))

(use-package
 eldoc-box
 :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
 :custom (eldoc-box-clear-with-C-g t))

(provide 'init-lsp)
;;; init-lsp.el ends here
