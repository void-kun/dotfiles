;;; lolo-lisp.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lolo-programming)
(lolo-require-packages '(rainbow-delimiters))

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; wrap keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (lolo-wrap-with "("))
;; FIXME: Pick terminal-friendly binding.
;;(define-key lisp-mode-shared-map (kbd "M-[") (lolo-wrap-with "["))
(define-key lisp-mode-shared-map (kbd "M-\"") (lolo-wrap-with "\""))

;; a great lisp coding hook
(defun lolo-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq lolo-lisp-coding-hook 'lolo-lisp-coding-defaults)

;; interactive modes don't need whitespace checks
(defun lolo-interactive-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq lolo-interactive-lisp-coding-hook 'lolo-interactive-lisp-coding-defaults)

(provide 'lolo-lisp)
;;; lolo-lisp.el ends here
