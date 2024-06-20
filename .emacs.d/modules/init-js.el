;;; init-js.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(lolo/require-packages '(js2-mode json-mode))

(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'"     . js2-mode))
(add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"    . js2-mode))
(add-to-list 'interpreter-mode-alist '("node"  . js2-mode))

(with-eval-after-load 'js2-mode
  (defun lolo/js-mode-defaults ()
    ;; electric-layout-mode doesn't play nice with smartparens
    (setq-local electric-layout-rules '((?\; . after)))
    (setq mode-name "JS2")
    (js2-imenu-extras-mode +1)
    (subword-mode +1))

  (setq lolo-js-mode-hook 'lolo/js-mode-defaults)

  (add-hook 'js2-mode-hook (lambda () (run-hooks 'lolo-js-mode-hook))))

(provide 'init-js)
;;; init-js.el ends here
