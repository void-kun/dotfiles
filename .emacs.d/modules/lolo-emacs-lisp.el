;;; lolo-emacs-lisp.el --------------------------------


(require 'lolo-lisp)
(require 'crux)

(lolo-require-packages '(elisp-slime-nav rainbow-mode))

(defun lolo-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (string-prefix-p lolo-dir (file-truename buffer-file-name))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))

(defun lolo-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'lolo-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(defun lolo-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun lolo-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'lolo-lisp-coding-hook)
  (eldoc-mode +1)
  (lolo-recompile-elc-on-save)
  (rainbow-mode +1)
  (setq mode-name "EL")
  (lolo-conditional-emacs-lisp-checker))

(setq lolo-emacs-lisp-mode-hook 'lolo-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'lolo-emacs-lisp-mode-hook)))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;; ielm is an interactive Emacs Lisp shell
(defun lolo-ielm-mode-defaults ()
  "Sensible defaults for `ielm'."
  (run-hooks 'lolo-interactive-lisp-coding-hook)
  (eldoc-mode +1))

(setq lolo-ielm-mode-hook 'lolo-ielm-mode-defaults)

(add-hook 'ielm-mode-hook (lambda ()
                            (run-hooks 'lolo-ielm-mode-hook)))

(with-eval-after-load "elisp-slime-nav"
  (diminish 'elisp-slime-nav-mode))
(with-eval-after-load "rainbow-mode"
  (diminish 'rainbow-mode))
(with-eval-after-load "eldoc"
  (diminish 'eldoc-mode))

(with-eval-after-load "ielm"
  (define-key ielm-map (kbd "M-(") (lolo-wrap-with "("))
  (define-key ielm-map (kbd "M-\"") (lolo-wrap-with "\"")))

;; enable elisp-slime-nav-mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)


(provide 'lolo-emacs-lisp)
;;; lolo-emacs-lisp.el ends here
