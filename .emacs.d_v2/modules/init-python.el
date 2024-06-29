;;; init-python.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 python-mode
 :ensure nil
 :after flycheck
 :mode "\\.py\\'"
 :custom
 (python-indent-offset 4)
 (flycheck-python-pycompile-executable "python3")
 (python-shell-interpreter "python3"))

(with-eval-after-load 'eglot
  ;; config python
  (add-to-list
   'eglot-server-programs
   '(python-mode . ("pyright-langserver" "--stdio"))))

(provide 'init-python)
;;; init-python.el ends here
