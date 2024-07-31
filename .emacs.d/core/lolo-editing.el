;; -*- coding: utf-8; lexical-binding: t -*-
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
(use-package
 smartparens
 :defer t
 :straight
 (smartparens :build t :type git :host github :repo "Fuco1/smartparens")
 :hook (prog-mode . smartparens-mode)
 :diminish smartparens-mode
 :custom (sp-escape-quotes-after-insert nil)
 :config
 ;; stop pairing single quotes in elisp
 (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
 (sp-local-pair 'org-mode "[" nil :actions nil))

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
(use-package crux
 :straight (:build t)
 :config
 (crux-with-region-or-buffer indent-region)
 (crux-with-region-or-buffer untabify)
 (crux-with-region-or-point-to-eol kill-ring-save)
 (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(provide 'lolo-editing)
;;; lolo-editing.el ends here
