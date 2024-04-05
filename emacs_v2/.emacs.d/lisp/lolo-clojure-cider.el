;;; lolo-clojure-cider.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(require 'lolo-clojure)

(when (maybe-require-package 'cider)
  (setq nrepl-popup-stacktraces nil)

  (add-hook 'clojure-ts-mode-hook #'cider-mode)

  (with-eval-after-load 'cider
    (add-hook 'cider-repl-mode-hook 'subword-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode))

  (require-package 'flycheck-clojure)
  (with-eval-after-load 'clojure-mode
    (with-eval-after-load 'cider
      (with-eval-after-load 'flycheck
        (flycheck-clojure-setup)))))

(provide 'lolo-clojure-cider)
;;; lolo-clojure-cider.el ends here
