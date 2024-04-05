;;; lolo-recentf.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))

(provide 'lolo-recentf)
;;; lolo-recentf.el ends here
