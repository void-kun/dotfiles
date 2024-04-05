;;; lolo-spelling.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(require 'ispell)

(when (executable-find ispell-program-name)
  ;; Add spell-checking in comments for all programming language modes
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-;") nil)
    (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))

(provide 'lolo-spelling)
;;; lolo-spelling.el ends here
