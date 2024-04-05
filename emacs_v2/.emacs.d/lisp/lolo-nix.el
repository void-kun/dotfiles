;;; lolo-nix.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(if (maybe-require-package 'nix-ts-mode)
    (when (and (fboundp 'treesit-ready-p) (treesit-ready-p 'nix t))
      (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode)))
  (maybe-require-package 'nix-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nil"))))

(maybe-require-package 'nixpkgs-fmt)


(provide 'lolo-nix)
;;; lolo-nix.el ends here
