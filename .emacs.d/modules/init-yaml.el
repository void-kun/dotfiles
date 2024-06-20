;;; init-yaml.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(lolo/require-packages '(yaml-mode))

(with-eval-after-load 'yaml-mode
    ;; yaml-mode doesn't derive from prog-mode, but we can at least enable
    ;; whitespace-mode and apply cleanup.
    (add-hook 'yaml-mode-hook 'whitespace-mode)
    (add-hook 'yaml-mode-hook 'subword-mode)
    (add-hook 'yaml-mode-hook
            (lambda () (add-hook 'before-save-hook 'lolo/cleanup-maybe nil t))))

(provide 'init-yaml)
;;; init-yaml.el ends here
