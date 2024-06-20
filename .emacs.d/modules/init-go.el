;;; init-go.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'init-lsp)

(lolo/require-packages '(go-mode
                         lsp-mode
                         lsp-ui
                         company
                         gotest))

;; Ignore go test -c output files
(add-to-list 'completion-ignored-extensions ".test")

(define-key 'help-command (kbd "G") 'godoc)

;; Fix: super-save will cause go files to be saved when lsp-mode does
;; certain things, triggering lsp-format-buffer. This causes, inter alia,
;; commas to disappear when typing go function invocations
(add-to-list 'super-save-predicates
             (lambda () (not (eq major-mode 'go-mode))))

(with-eval-after-load 'go-mode
  (defun lolo/go-mode-defaults ()
    ;; Add to default go-mode key bindings
    (let ((map go-mode-map))
      (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
      (define-key map (kbd "C-c m") 'go-test-current-file)
      (define-key map (kbd "C-c .") 'go-test-current-test)
      (define-key map (kbd "C-c b") 'go-run)
      (define-key map (kbd "C-h f") 'godoc-at-point))

    ;; Prefer goimports to gofmt if installed
    (let ((goimports (executable-find "goimports")))
      (when goimports
        (setq gofmt-command goimports)))

    ;; stop whitespace being highlighted
    (whitespace-toggle-options '(tabs))

    ;; CamelCase aware editing operations
    (subword-mode +1))

  ;; if yas is present, this enables yas-global-mode
  ;; which provides completion via company
  (if (fboundp 'yas-global-mode)
      (yas-global-mode))

  ;; configure lsp for go
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'lsp-deferred)

  (setq lolo-go-mode-hook 'lolo/go-mode-defaults)
  (add-hook 'go-mode-hook (lambda ()
                            (run-hooks 'lolo-go-mode-hook))))

(provide 'init-go)
;;; init-go.el ends here
