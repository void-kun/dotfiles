;;; lolo-programming.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package rainbow-mode
  :ensure t)

(use-package eglot
  :ensure t
  :demand t
  :general
  (general-define-key :keymaps 'eglot-mode-map
					  "C-c f" 'eglot-format-buffer
					  "C-c a" 'eglot-code-actions
					  "C-c d" 'eldoc
					  "C-c r" 'eglot-rename)
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package eldoc
  :ensure t
  :custom (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit))

(provide 'lolo-programming)
;;; lolo-programming.el ends here
