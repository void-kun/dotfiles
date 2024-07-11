;;; init-python.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(with-eval-after-load 'eglot
  ;; config python
  (add-to-list
   'eglot-server-programs
   '(python-ts-mode . ("pyright-langserver" "--stdio"))))

(provide 'init-python)
;;; init-python.el ends here
