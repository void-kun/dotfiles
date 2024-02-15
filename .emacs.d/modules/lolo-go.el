;;; lolo-go.el --------------------------------

(require 'lolo-programming)
(require 'lolo-lsp)

;;; Code:
(lolo-require-packages '(go-mode
                         go-projectile
                         lsp-mode
                         lsp-ui
                         company
                         gotest))

(require 'go-projectile)

;; pkg go installation
(setq exec-path (append '("/usr/local/go/bin") exec-path))
(setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))

;; error highlight
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ignore go test -c output files

(add-to-list 'completion-ignored-extensions ".test")
(define-key 'help-command (kbd "G") 'godoc)

(require 'tree-sitter)
(require 'tree-sitter-langs)

;; config go lang
(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook #'tree-sitter-mode)
  (add-hook 'go-mode-hook #'tree-sitter-hl-mode)

  ;; setup go-lsp mode
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook 'lsp-go-install-save-hooks)


  (defun mapping-go-mode-hook()
    (setq tab-width 4 indent-tabs-mode 1)
    (go-eldoc-setup)
    (local-set-key (kbd "M-.") #'godef-jump)
    (add-hook 'before-save-hook 'gofmt-before-save)

    ;; mapping
    (let ((map go-mode-map))
         (define-key map (kbd "C-c a") 'go-test-current-project)
         (define-key map (kbd "C-c m") 'go-test-current-file)
         (define-key map (kbd "C-c .") 'go-test-current-test)
         (define-key map (kbd "C-c b") 'go-run)))
  (add-hook 'go-mode-hook 'mapping-go-mode-hook)

  ;; start lsp mode
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'yas-minor-mode))

(provide 'lolo-go)
;;; lolo-go.el ends here
