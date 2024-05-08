;;; lolo-kill-ring.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(setq kill-ring-max 200)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Kill & Mark things easily
(use-package
 easy-kill
 :bind
 (([remap kill-ring-save] . easy-kill) ([remap mark-sexp] . easy-mark)))

;; Interactively insert and edit items from kill-ring
(use-package
 browse-kill-ring
 :bind ("C-c k" . browse-kill-ring)
 :hook (after-init . browse-kill-ring-default-keybindings)
 :init
 (setq
  browse-kill-ring-separator "────────────────"
  browse-kill-ring-separator-face 'shadow))

(provide 'lolo-kill-ring)
;;; lolo-kill-ring.el ends here
