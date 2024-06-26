;;; init-cpp.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package clang-format)

(use-package
 modern-cpp-font-lock
 :diminish t
 :init (modern-c++-font-lock-global-mode t))

(use-package cmake-mode :defer t)

(provide 'init-cpp)
;;; init-cpp.el ends here
