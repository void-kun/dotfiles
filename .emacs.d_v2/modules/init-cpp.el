;;; init-cpp.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp))))
(setq ccls-executable "/usr/bin/ccls")
(setq ccls-sem-highlight-method 'font-lock)
;; (setq ccls-use-default-rainbow-sem-highlight)
(ccls-code-lens-mode 1)

(use-package clang-format)

(use-package
 modern-cpp-font-lock
 :diminish t
 :init (modern-c++-font-lock-global-mode t))

(use-package cmake-mode :defer t)

(provide 'init-cpp)
;;; init-cpp.el ends here
