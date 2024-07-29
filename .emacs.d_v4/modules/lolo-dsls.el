;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(defun my/local-tab-indent ()
  (setq-local indent-tabs-mode 1))

(add-hook 'makefile-mode-hook #'my/local-tab-indent)

;; ============================================================================
(use-package caddyfile-mode
  :defer t
  :straight (:build t)
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

;; ============================================================================
(use-package cmake-mode
  :defer t
  :straight (:build t))

(use-package company-cmake
  :straight (company-cmake :build t
                           :type git
                           :host github
                           :repo "purcell/company-cmake")
  :after cmake-mode
  :defer t)

(use-package cmake-font-lock
  :defer t
  :after cmake-mode
  :straight (:build t))

(use-package eldoc-cmake
  :straight (:build t)
  :defer t
  :after cmake-mode)

;; ============================================================================
(use-package csv-mode
  :straight (:build t)
  :defer t)

;; ============================================================================
(use-package dotenv-mode
  :defer t
  :straight (:build t))

;; ============================================================================
(use-package gnuplot
  :straight (:build t)
  :defer t)

;; ============================================================================
(use-package graphviz-dot-mode
  :defer t
  :straight (:build t)
  :after org
  :mode (("\\.diag\\'"      . graphviz-dot-mode)
         ("\\.blockdiag\\'" . graphviz-dot-mode)
         ("\\.nwdiag\\'"    . graphviz-dot-mode)
         ("\\.rackdiag\\'"  . graphviz-dot-mode)
         ("\\.dot\\'"       . graphviz-dot-mode)
         ("\\.gv\\'"        . graphviz-dot-mode))
  :init
  (setq graphviz-dot-indent-width tab-width)
  (with-eval-after-load 'org
      (defalias 'org-babel-execute:graphviz-dot #'org-babel-execute:dot)
      (add-to-list 'org-babel-load-languages '(dot . t))
      (require 'ob-dot)
      (setq org-src-lang-modes
            (append '(("dot" . graphviz-dot))
                    (delete '("dot" . fundamental) org-src-lang-modes))))
  :config
  (setq graphviz-dot-indent-width 4))

;; ============================================================================
(use-package markdown-mode
  :defer t
  :straight t
  :mode
  (("\\.mkd\\'" . markdown-mode)
   ("\\.mdk\\'" . markdown-mode)
   ("\\.mdx\\'" . markdown-mode))
  :hook (markdown-mode . orgtbl-mode)
  :hook (markdown-mode . visual-line-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package gh-md
  :defer t
  :after markdown-mode
  :straight (:build t))

(use-package ox-gfm
  :straight (:build t)
  :defer t
  :after (org ox))

(use-package markdown-toc
  :defer t
  :after markdown-mode
  :straight (:build t))

(use-package edit-indirect
  :straight (:build t)
  :defer t)

;; ============================================================================
(use-package nix-mode
  :straight (:build t)
  :defer t)

;; ============================================================================
(use-package nginx-mode
  :straight (:build t)
  :defer t)

(use-package company-nginx
  :straight (company-nginx :build t
                           :type git
                           :host github
                           :repo "emacsmirror/company-nginx")
  :defer t
  :config
  (add-hook 'nginx-mode-hook (lambda ()
                               (add-to-list 'company-backends #'company-nginx))))

;; ============================================================================
(use-package pkgbuild-mode
  :straight (:build t)
  :defer t
  :custom
  (pkgbuild-update-sums-on-save nil)
  (pkgbuild-ask-about-save nil))

;; ============================================================================
(use-package plantuml-mode
  :defer t
  :straight (:build t)
  :mode ("\\.\\(pum\\|puml\\)\\'" . plantuml-mode)
  :after ob
  :init
  (add-to-list 'org-babel-load-languages '(plantuml . t))
  :config
  (setq plantuml-default-exec-mode 'jar
        plantuml-jar-path "~/.local/bin/plantuml.jar"
        org-plantuml-jar-path "~/.local/bin/plantuml.jar"))

;; ============================================================================
(use-package shell
  :defer t
  :straight (:type built-in)
  :hook (shell-mode . tree-sitter-hl-mode))

;; ============================================================================
(use-package ssh-config-mode
  :defer t
  :straight (:build t))

;; ============================================================================
(use-package systemd
  :defer t
  :straight (:build t))

;; ============================================================================
(use-package tmux-mode
  :defer t
  :straight (tmux-mode :type git :host github :repo "nverno/tmux-mode")
  :mode (("tmux\\.conf\\'" . tmux-mode)))

;; ============================================================================
(use-package toml-mode
  :straight (:build t)
  :defer t
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

;; ============================================================================
(use-package yaml-mode
  :defer t
  :straight (:build t)
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

;; ============================================================================
(use-package yuck-mode
  :straight (:build t)
  :defer t
  :hook ((yuck-mode . parinfer-rust-mode)))

(provide 'lolo-dsls)
;;; lolo-dsls.el ends here
