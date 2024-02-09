;;; lolo-lisp.el --------------------------------

(require 'lolo-programming)
(lolo-require-packages '(rainbow-delimiters))

;; lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; wrap keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (lolo-wrap-with "("))
(define-key lisp-mode-shared-map (kbd "M-\"") (lolo-wrap-with "\""))

(defun lolo-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq lolo-lisp-coding-hook 'lolo-lisp-coding-defaults)

(defun lolo-interactive-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq lolo-interactive-lisp-coding-hook 'lolo-interactive-lisp-coding-defaults)

(provide 'lolo-lisp)
;;; lolo-lisp.el ends here
