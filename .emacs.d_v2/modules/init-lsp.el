;;; init-lsp.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; define treesitter language source list
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
   (rust "https://github.com/tree-sitter/tree-sitter-rust")
   (c "https://github.com/tree-sitter/tree-sitter-c")
   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (rust-mode . rust-ts-mode)
   (c-mode . c-ts-mode)
   (c++-mode . c++-ts-mode)
   (go-mode . go-ts-mode)
   (python-mode . python-ts-mode)))

;; ============================================================================
;; lsp config.
(use-package lsp-mode
  :init
  (setq lsp-auto-guess-root nil)
  (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-ts-mode . lsp-deferred)
	 (python-ts-mode . lsp-deferred)
	 (c-ts-mode . lsp-deferred)
	 (c++-ts-mode . lsp-deferred)
	 (go-ts-mode . lsp-deferred)
	 ;; intergration which-key
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-idle-delay 0.100)
  (setq lsp-log-io nil)
  (setq read-process-output-max (* 3 1024 1024)) ;; 1mb
)

;; golang-mode
(defun lsp-go-install-save-hooks ()
  "Format and import when saved file."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-ts-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-ui
  :commands lsp-ui-mode)

;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; debugger
(use-package dap-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
