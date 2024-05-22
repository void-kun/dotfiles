;;; lolo-go.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:


(eval-when-compile
  (require 'lolo-custom))

;; Golang
(use-package
  go-mode
  :autoload godoc-gogetdoc
  :bind (:map go-mode-map ("<f1>" . godoc))
  :init (setq godoc-at-point-function #'godoc-gogetdoc)

  ;; Install tools
  (defconst go--tools
    '("golang.org/x/tools/gopls"
      "golang.org/x/tools/cmd/goimports"
      "honnef.co/go/tools/cmd/staticcheck"
      "github.com/go-delve/delve/cmd/dlv"
      "github.com/zmb3/gogetdoc"
      "github.com/josharian/impl"
      "github.com/cweill/gotests/..."
      "github.com/fatih/gomodifytags"
      "github.com/davidrjenni/reftools/cmd/fillstruct")
    "All necessary go tools.")

  (defun go-install-tools ()
    "Install or update go tools."
    (interactive)

    (message "Installing go tools...")
    (dolist (pkg go--tools)
      (set-process-sentinel
       (start-process "go-tools" "*Go Tools*" "go"
                      "install"
                      (concat pkg "@latest"))
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed %s" pkg)
             (message "Failed to install %s: %d" pkg status)))))))
  :config

  ;; Try to install go tools if `gopls' is not found
  (unless (executable-find "gopls")
    (go-install-tools))

  ;; Misc
  (use-package go-dlv) (use-package go-fill-struct) (use-package go-impl)

  (use-package
    go-tag
    :bind
    (:map go-mode-map ("C-c t a" . go-tag-add) ("C-c t r" . go-tag-remove))
    :init (setq go-tag-args (list "-transform" "camelcase")))

  (use-package
    go-gen-test
    :bind (:map go-mode-map ("C-c t g" . go-gen-test-dwim)))

  (use-package
    gotest
    :bind
    (:map
     go-mode-map
     ("C-c t f" . go-test-current-file)
     ("C-c t t" . go-test-current-test)
     ("C-c t j" . go-test-current-project)
     ("C-c t b" . go-test-current-benchmark)
     ("C-c t c" . go-test-current-coverage)
     ("C-c t x" . go-run))))

(when (lolo-treesit-available-p)
  (use-package go-ts-mode :init (setq go-ts-mode-indent-offset 4)))

(provide 'lolo-go)
;;; lolo-go.el ends here
