;;; init-terminal.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package shell-here
  :bind ("M-~" . shell-here)
  :config
  (when *sys/linux*
    (setq explicit-shell-file-name "/usr/bin/zsh")))

(use-package term-keys
  :if (not (display-graphic-p))
  :config (term-keys-mode t))

(provide 'init-terminal)
;;; init-terminal.el ends here
