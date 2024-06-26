;;; init-projectile.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 projectile
 :custom (projectile-completion-system 'ivy)
 :config
 (projectile-mode 1)
 (add-to-list
  'projectile-globally-ignored-directories "node_modules")
 (setq projectile-track-known-projects-automatically nil))

(provide 'init-projectile)
;;; init-projectile.el ends here
