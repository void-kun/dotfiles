;;; lolo-org.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-habit)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; a few useful global keybindings for org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)

(setq org-log-done t)
(setq org-log-into-drawer t)

(defun lolo-org-mode-defaults ()
  (let ((oldmap (cdr (assoc 'lolo-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-c +") nil)
    (define-key newmap (kbd "C-c -") nil)
    (define-key newmap (kbd "C-a") 'org-beginning-of-line)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(lolo-mode . ,newmap) minor-mode-overriding-map-alist))
)

(setq lolo-org-mode-hook 'lolo-org-mode-defaults)

(add-hook 'org-mode-hook (lambda () (run-hooks 'lolo-org-mode-hook)))

(provide 'lolo-org)
;;; lolo-org.el ends here
