;;; lolo-go.el --------------------------------

(require 'lolo-programming)
(require 'lolo-lsp)

(lolo-require-packages '(go-mode
                         go-projectile
                         lsp-mode
                         lsp-ui
                         company
                         gotest))

(require 'go-projectile)

;; ignore go test -c output files

(add-to-list 'completion-ignored-extensions ".test")
(define-key 'help-command (kbd "G") 'godoc)

(add-to-list 'super-save-predicates
             (lambda () (not (eq major-mode 'go-mode))))

(with-eval-after-load 'go-mode
  (defun lolo-go-mode-defaults ()
    ;; Add to default go-mode key bindings
    (let ((map go-mode-map))
      (define-key map (kbd "C-c a") 'go-test-current-project)
      (define-key map (kbd "C-c m") 'go-test-current-file)
      (define-key map (kbd "C-c ,") 'go-test-current-test)
      (define-key map (kbd "C-c b") 'go-run)
      (define-key map (kbd "C-h f") 'godoc-at-point))

    ;; Prefer goimports to gofmt if installed
    (let ((goimports (executable-find "goimports")))
      (when goimports
        (setq gofmt-command goimports)))

    (whitespace-toggle-options '(tabs))

    (subword-mode +1)))

(if (fboundp 'yas-global-mode)
    (yas-global-mode))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lolo-go-mode-defaults)
(add-hook 'go-mode-hook #'lsp-deferred)

(setq lolo-go-mode-hook 'lolo-go-mode-defaults)
(add-hook 'go-mode-hook (lambda () (run-hooks 'lolo-go-mode-hook)))

(provide 'lolo-go)
;;; lolo-go.el ends here
