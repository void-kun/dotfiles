;;; lolo-programming.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun lolo-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; font-lock annotations like TODO in source code
(require 'hl-todo)
(global-hl-todo-mode 1)

;; smart curly braces
(sp-pair "{" nil :post-handlers
         '(((lambda (&rest _ignored)
              (crux-smart-open-line-above)) "RET")))

;; enlist a more liberal guru
(setq guru-warn-only t)

(defun lolo-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (when (and (executable-find ispell-program-name)
             lolo-flyspell)
    (flyspell-prog-mode))
  (when lolo-guru
    (guru-mode +1)
    (diminish 'guru-mode))
  (smartparens-mode +1)
  (lolo-enable-whitespace)
  (lolo-local-comment-auto-fill))

(setq lolo-prog-mode-hook 'lolo-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'lolo-prog-mode-hook)))

;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(provide 'lolo-programming)
;;; lolo-programming.el ends here
