;;; init-evil.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 evil
 :ensure t
 :init (setq evil-want-intergration t) (setq evil-want-keybinding nil)
 :config (evil-mode 1))

(use-package
 evil-collection
 :after evil
 :ensure t
 :config (evil-collection-init))

(provide 'init-evil)
;;; init-evil.el ends here
