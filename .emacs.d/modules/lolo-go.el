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

;; ignore go test -c output files

(add-to-list 'completion-ignored-extensions ".test")
(define-key 'help-command (kbd "G") 'godoc)

;; Golang
(use-package go-mode
  :functions (go-install-tools exec-path-from-shell-copy-envs)
  :autoload godoc-gogetdoc
  :bind (:map go-mode-map
              ("<f1>" . godoc))
  :init
  (setq godoc-at-point-function #'godoc-gogetdoc)

  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  ;; Misc
  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)

  (use-package go-tag
    :bind (:map go-mode-map
                ("C-c t a" . go-tag-add)
                ("C-c t r" . go-tag-remove))
    :init (setq go-tag-args (list "-transform" "camelcase")))

  (use-package go-gen-test
    :bind (:map go-mode-map
                ("C-c t g" . go-gen-test-dwim)))

  (use-package gotest
    :bind (:map go-mode-map
                ("C-c t f" . go-test-current-file)
                ("C-c t t" . go-test-current-test)
                ("C-c t j" . go-test-current-project)
                ("C-c t b" . go-test-current-benchmark)


                ("C-c t c" . go-test-current-coverage)
                ("C-c t x" . go-))))

(provide 'lolo-go)
;;; lolo-go.el ends here
