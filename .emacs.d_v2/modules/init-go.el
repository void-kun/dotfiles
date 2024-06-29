;;; init-go.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 go-mode
 :mode "\\.go\\'"
 :hook (before-save . gofmt-before-save)
 :custom (gofmt-command "goimports"))

(require 'project)

(with-eval-after-load 'eglot
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module))

(with-eval-after-load 'eglot
  ;; config golang
  (setq-default eglot-workspace-configuration
                '((:gopls
                   .
                   ((staticcheck . t) (matcher . "CaseSensitive")))))
  (add-hook 'go-mode-hook 'eglot-ensure)
  (defun eglot-format-buffer-before-save-go ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
  (add-hook 'go-mode-hook #'eglot-format-buffer-before-save-go))

(provide 'init-go)
;;; init-go.el ends here
