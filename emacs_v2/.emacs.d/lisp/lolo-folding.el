;;; lolo-folding.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(when (maybe-require-package 'origami)
  (with-eval-after-load 'origami
    (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
    (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes)))

(provide 'lolo-folding)
;;; lolo-folding.el ends here
