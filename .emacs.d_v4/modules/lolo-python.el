;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package python
  :defer t
  :straight (:build t)
  :after ob
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)
         ("[./]flake8\\'" . conf-mode)
         ("/Pipfile\\'"   . conf-mode))
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  (add-hook 'python-mode-local-vars-hook #'lsp)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (when (and (executable-find "python3")
           (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3")))

(use-package pytest
  :defer t
  :straight (:build t)
  :commands (pytest-one
             pytest-pdb-one
             pytest-all
             pytest-pdb-all
             pytest-last-failed
             pytest-pdb-last-failed
             pytest-module
             pytest-pdb-module)
  :config
  (add-to-list 'pytest-project-root-files "setup.cfg"))

(use-package poetry
  :defer t
  :straight (:build t)
  :commands (poetry-venv-toggle
             poetry-tracking-mode)
  :config
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(use-package pip-requirements
  :defer t
  :straight (:build t))

(use-package pippel
  :defer t
  :straight (:build t))

(use-package pipenv
  :defer t
  :straight (:build t)
  :commands (pipenv-activate
             pipenv-deactivate
             pipenv-shell
             pipenv-open
             pipenv-install
             pipenv-uninstall)
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-with-projectile nil))

(use-package pyenv
  :defer t
  :straight (:build t)
  :config
  (add-hook 'python-mode-hook #'pyenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyenv-virtual-env-name (" venv:" pyenv-virtual-env-name " "))
               'append))

(use-package pyenv-mode
  :defer t
  :after python
  :straight (:build t)
  :if (executable-find "pyenv")
  :commands (pyenv-mode-versions))

(use-package pyimport
  :defer t
  :straight (:build t))

(use-package py-isort
  :defer t
  :straight (:build t))

(use-package counsel-pydoc
  :defer t
  :straight (:build t))

(use-package sphinx-doc
  :defer t
  :straight (:build t)
  :init
  (add-hook 'python-mode-hook #'sphinx-doc-mode))

(use-package cython-mode
  :defer t
  :straight (:build t)
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s"))

(use-package flycheck-cython
  :defer t
  :straight (:build t)
  :after cython-mode)

(use-package blacken
  :defer t
  :straight (:build t)
  :init
  (add-hook 'python-mode-hook #'blacken-mode))

(use-package lsp-pyright
  :after lsp-mode
  :defer t
  :straight (:buidl t))

(provide 'lolo-python)
;;; lolo-python.el ends here
