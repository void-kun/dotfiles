;;; lolo-python.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

;; A better python mode (supposedly)
(use-package python-mode
  :defer t)

(general-define-key :states '(emacs) :keymaps 'inferior-python-mode-map
                    "<up>" 'comint-previous-input
                    "<down>" 'comint-next-input)

;; Using my virtual environments
(use-package pyvenv
  :defer t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")) ;; Where the virtual envs are stored on my computer


;; Automatically set the virtual environment when entering a directory
(use-package auto-virtualenv
  :defer 2
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

;; Python development helper
;; (use-package elpy
;;   :defer t
;;   :init
;;   (setq elpy-rpc-virtualenv-path 'current)
;;   (advice-add 'python-mode :before 'elpy-enable))

(provide 'lolo-python)
;;; lolo-python.el ends here
