;;; init-lsp.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(setq treesit-language-source-alist
 '((bash "https://github.com/tree-sitter/tree-sitter-bash")
   (cmake "https://github.com/uyha/tree-sitter-cmake")
   (css "https://github.com/tree-sitter/tree-sitter-css")
   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
   (go "https://github.com/tree-sitter/tree-sitter-go")
   (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
   (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
   (html "https://github.com/tree-sitter/tree-sitter-html")
   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (make "https://github.com/alemuller/tree-sitter-make")
   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
   (python "https://github.com/tree-sitter/tree-sitter-python")
   (toml "https://github.com/tree-sitter/tree-sitter-toml")
   (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package eglot
  :commands eglot
  :init
  (setq eglot-stay-out-of '(flymake))
  :custom
  (eglot-ignored-server-capabilites '(:documentHighlightProvider))
  (eglot-autoshutdown t)
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode))

(with-eval-after-load 'eglot
    (load-library "project")
    ;; config c/c++
    (add-to-list 'eglot-server-programs
                 `((c++-mode c-mode c-ts-mode c++-ts-mode) ,"clangd" ,"--query-driver=/**/*"))
    (add-hook 'c-mode-hook 'eglot-ensure)
    (add-hook 'c-ts-mode-hook 'eglot-ensure)
    (add-hook 'c++-mode-hook 'eglot-ensure)
    (add-hook 'c++-ts-mode-hook 'eglot-ensure)
    ;; config python
    (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
    ;; config rust
    (add-to-list 'eglot-server-programs
                       `(rust-mode . ("rust-analyzer" :initializationOptions
                                     ( :procMacro (:enable t)
                                       :cargo ( :buildScripts (:enable t)
                                                :features "all"))))))

(use-package eldoc-box
  :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-clear-with-C-g t))

(provide 'init-lsp)
;;; init-lsp.el ends here
