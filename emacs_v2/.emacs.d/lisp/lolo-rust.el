;;; lolo-rust.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:


(when (maybe-require-package 'rust-mode)
  (when (maybe-require-package 'flycheck-rust)
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
      (add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                               (rust-format-buffer)))))))



(provide 'lolo-rust)
;;; lolo-rust.el ends here
