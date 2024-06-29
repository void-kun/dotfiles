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

(with-eval-after-load 'eglot
  ;; config c/c++
  (add-to-list
   'eglot-server-programs
   `((c++-mode c-mode c-ts-mode c++-ts-mode)
     ,
     "clangd"
     ,
     "--query-driver=/**/*"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure))

(provide 'init-cpp)
;;; init-cpp.el ends here
