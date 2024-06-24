;;; init-projectile.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 projectile
 :bind ("C-x p" . projectile-command-map)
 :custom (projectile-completion-system 'ivy)
 :config (projectile-mode 1)
 (when (and *sys/win32* (executable-find "tr"))
   (setq projectile-indexing-method 'alien))
 (add-to-list
  'projectile-globally-ignored-directories "node_modules"))

(provide 'init-projectile)
;;; init-projectile.el ends here
