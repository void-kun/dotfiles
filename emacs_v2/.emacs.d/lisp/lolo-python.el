;;; lolo-python.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python")

(require-package 'pip-requirements)

(when (maybe-require-package 'flymake-ruff)
  (defun sanityinc/flymake-ruff-maybe-enable ()
    (when (executable-find "ruff")
      (flymake-ruff-load)))
  (add-hook 'python-mode-hook 'sanityinc/flymake-ruff-maybe-enable))

(maybe-require-package 'ruff-format)

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))

(provide 'lolo-python)
;;; lolo-python.el ends here
    