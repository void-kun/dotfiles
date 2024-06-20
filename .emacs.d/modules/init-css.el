;;; init-css.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(with-eval-after-load 'css-mode
  (lolo/require-packages '(rainbow-mode))

  (setq css-indent-offset 2)

  (defun prelude-css-mode-defaults ()
    (rainbow-mode +1)
    (run-hooks 'prelude-prog-mode-hook))

  (setq prelude-css-mode-hook 'prelude-css-mode-defaults)

  (add-hook 'css-mode-hook (lambda ()
                             (run-hooks 'prelude-css-mode-hook))))

(provide 'init-css)
;;; init-css.el ends here
