;;; lolo-clojure.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(when (or (maybe-require-package 'clojure-ts-mode)
          (maybe-require-package 'clojure-mode))
  (require-package 'cljsbuild-mode)
  (require-package 'elein)

  (with-eval-after-load 'clojure-mode
    (dolist (m '(clojure-mode-hook clojure-ts-mode-hook))
      (add-hook m 'sanityinc/lisp-setup))))
      
(provide 'lolo-clojure)
;;; lolo-clojure.el ends here
