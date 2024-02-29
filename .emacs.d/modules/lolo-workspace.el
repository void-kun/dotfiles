
;;; lolo-workspace.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom
  (tab-bar-show nil)

  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t))
        
(provide 'lolo-workspace)
;;; lolo-workspace.el ends here
