;;; lolo-projectile.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 projectile
 :config
 (projectile-mode 1)
 (add-to-list
  'projectile-globally-ignored-directories "node_modules")
 (setq projectile-track-known-projects-automatically nil))

(provide 'lolo-projectile)
;;; lolo-projectile.el ends here
