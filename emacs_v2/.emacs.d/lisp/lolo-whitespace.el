;;; lolo-whitespace.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(setq-default show-trailing-whitespace nil)

;;; Whitespace

(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'sanityinc/show-trailing-whitespace))


(require-package 'whitespace-cleanup-mode)
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
(with-eval-after-load 'whitespace-cleanup-mode
  (diminish 'whitespace-cleanup-mode))

(global-set-key [remap just-one-space] 'cycle-spacing)

(provide 'lolo-whitespace)
;;; lolo-whitespace.el ends here
