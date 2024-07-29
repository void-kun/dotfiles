;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package
 atomic-chrome
 :straight (:build t)
 :init (atomic-chrome-start-server)
 :config
 (setq
  atomic-chrome-default-major-mode 'markdown-mode
  atomic-chrome-url-major-mode-alist
  `(("github\\.com" . gfm-mode)
    ("gitlab\\.com" . gfm-mode)
    ("reddit\\.com" . markdown-mode))))

;; ============================================================================
(use-package
 editorconfig
 :defer t
 :straight (:build t)
 :diminish editorconfig-mode
 :init (editorconfig-mode t))

;; ============================================================================
(use-package smartparens :straight (:build t) :defer t)

;; ============================================================================
(use-package
 parinfer-rust-mode
 :defer t
 :straight (:build t)
 :diminish parinfer-rust-mode
 :hook emacs-lisp-mode common-lisp-mode scheme-mode
 :init
 (setq
  parinfer-rust-auto-download t
  parinfer-rust-library-directory (concat user-emacs-directory "parinfer-rust/"))
 (add-hook 'parinfer-rust-mode-hook (lambda () (smartparens-mode -1))))

;; ============================================================================
(use-package
 smartparens
 :defer t
 :straight
 (smartparens :build t :type git :host github :repo "Fuco1/smartparens")
 :hook (prog-mode . smartparens-mode))

;; ============================================================================
(use-package string-edit-at-point :defer t :straight (:build t))

;; ============================================================================
(use-package
 writeroom-mode
 :defer t
 :straight (:build t)
 :init (global-writeroom-mode 1)
 :config
 (setq
  writeroom-width 100
  writeroom-fullscreen-effect nil
  writeroom-maximize-window nil
  writeroom-mode-line t
  writeroom-major-modes '(text-mode org-mode markdown-mode nov-mode Info-mode)))

;; ============================================================================
(use-package
 crux
 :config
 (crux-with-region-or-buffer indent-region)
 (crux-with-region-or-buffer untabify)
 (crux-with-region-or-point-to-eol kill-ring-save)
 (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(provide 'lolo-editing)
;;; lolo-editing.el ends here
