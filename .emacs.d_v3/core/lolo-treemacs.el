;;; lolo-treemacs.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 treemacs
 :commands (treemacs-follow-mode treemacs-filewatch-mode treemacs-git-mode)
 :custom-face (cfrs-border-color ((t (:inherit posframe-border))))
 :bind
 (([f8] . treemacs)
  ("M-0" . treemacs-select-window)
  ("C-x t 1" . treemacs-delete-other-windows)
  ("C-x t t" . treemacs)
  ("C-x t b" . treemacs-bookmark)
  ("C-x t C-t" . treemacs-find-file)
  ("C-x t M-t" . treemacs-find-tag)
  :map
  treemacs-mode-map
  ([mouse-1] . treemacs-single-click-expand-action))
 :config
 (setq
  treemacs-collapse-dirs
  (if treemacs-python-executable
      3
    0)
  treemacs-missing-project-action 'remove
  treemacs-sorting 'alphabetic-asc
  treemacs-follow-after-init t
  treemacs-width 30
  treemacs-no-png-images nil)

 (treemacs-follow-mode t) (treemacs-filewatch-mode t)
 (pcase (cons
         (not (null (executable-find "git")))
         (not (null (executable-find "python3.11"))))
   (`(t . t) (treemacs-git-mode 'deferred))
   (`(t . _) (treemacs-git-mode 'simple)))

 (use-package
  treemacs-magit
  :hook
  ((magit-post-commit
    git-commit-post-finish magit-post-stage magit-post-unstage)
   . treemacs-magit--schedule-update))

 (use-package
  treemacs-tab-bar
  :demand t
  :config (treemacs-set-scope-type 'Tabs)))

(provide 'lolo-treemacs)
;;; lolo-treemacs.el ends here
