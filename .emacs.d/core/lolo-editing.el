;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package
 editorconfig
 :defer t
 :diminish editorconfig-mode
 :init (editorconfig-mode t))

;; ============================================================================
(use-package
 smartparens
 :defer t
 :hook (prog-mode . smartparens-mode)
 :diminish smartparens-mode
 :custom (sp-escape-quotes-after-insert nil)
 :config
 ;; stop pairing single quotes in elisp
 (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
 (sp-local-pair 'org-mode "[" nil :actions nil))

;; ============================================================================
(use-package string-edit-at-point :defer t)

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
