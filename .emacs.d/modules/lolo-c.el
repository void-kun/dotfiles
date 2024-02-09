;;; lolo-c.el --------------------------------

(require 'lolo-programming)

(defun lolo-c-mode-common-defaults ()
  (setq c-default-style "k&r"
        c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(setq lolo-c-mode-common-hook 'lolo-c-mode-common-defaults)

(add-hook 'c-mode-common-hook (lambda () (run hooks 'lolo-c-mode-common-hook)))

(defun lolo-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(setq lolo-makefile-mode-hook 'lolo-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda () (run-hooks 'lolo-makefile-mode-hook)))

(provide 'lolo-c)
;;; lolo-c.el ends here
