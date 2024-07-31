;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package
 recentf
 :straight (:build t :type built-in)
 :custom ((recentf-max-saved-items 2000))
 :config
 (lolo/add-all-to-list
  'recentf-exclude
  `(,(rx
      (* any)
      (or "elfeed-db"
          "eln-cache"
          "conlanging/content"
          "org/config"
          ".cache/")
      (* any) (?  (or "html" "pdf" "tex" "epub")))
    ,(rx (* any) ".elc" eol)
    ,(rx "/" (or "rsync" "ssh" "tmp" "yadm" "sudoedit" "sudo") (* any)))))

;; ============================================================================
(use-package
 company
 :straight (:build t)
 :defer t
 :init (global-company-mode)
 :config
 (setq
  company-minimum-prefix-length 2
  company-toolsip-limit 14
  company-tooltip-align-annotations t
  company-require-match 'never
  company-global-modes '(not erc-mode message-mode help-mode gud-mode)
  company-frontends
  '(company-pseudo-tooltip-frontend ; always show candidates in overlay tooltip
    company-echo-metadata-frontend) ; show selected candidate docs in echo area
  company-backends '(company-capf)
  company-auto-commit nil
  company-auto-complete-chars nil
  company-dabbrev-other-buffers nil
  company-dabbrev-ignore-case nil
  company-dabbrev-downcase nil))

(use-package
 company-dict
 :after company
 :straight (:build t)
 :config (setq company-dict-dir (expand-file-name "dicts" lolo-dir)))

(use-package
 company-box
 :straight (:build t)
 :after (company all-the-icons)
 :config
 (setq
  company-box-show-single-candidate t
  company-box-backends-colors nil
  company-box-max-candidates 50
  company-box-icons-alist 'company-box-icons-all-the-icons
  all-the-icons-scale-factor 0.8))

;; ============================================================================
;; Support Pinyin
(use-package
 pinyinlib
 :straight (:build t)
 :after orderless
 :autoload pinyinlib-build-regexp-string
 :init
 (defun completion--regex-pinyin (str)
   (orderless-regexp (pinyinlib-build-regexp-string str)))
 (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(use-package
 vertico
 :straight (:build t)
 :bind
 (:map
  vertico-map
  ("RET" . vertico-directory-enter)
  ("DEL" . vertico-directory-delete-char)
  ("M-DEL" . vertico-directory-delete-word))
 :hook
 ((after-init . vertico-mode)
  (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package
  vertico-posframe
  :straight (:build t)
  :hook (vertico-mode . vertico-posframe-mode)
  :init
  (setq
   vertico-posframe-poshandler
   #'posframe-poshandler-frame-center-near-bottom
   vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8))))

(use-package
 nerd-icons-completion
 :straight (:build t)
 :hook (vertico-mode . nerd-icons-completion-mode))

(use-package marginalia :hook (after-init . marginalia-mode))

;; ============================================================================
(use-package
 counsel
 :straight (:build t)
 :bind
 (("M-x" . counsel-M-x)
  ("C-x b" . counsel-ibuffer)
  ("C-x C-f" . counsel-find-file)
  :map
  minibuffer-local-map
  ("C-r" . 'counsel-minibuffer-history))
 :config
 (setq ivy-initial-inputs-alist nil))

(use-package
 yasnippet
 :defer t
 :straight (:build t)
 :init
 (use-package yasnippet-snippets :straight (:build t) :after yasnippet)
 (yas-global-mode)
 :hook ((prog-mode . yas-minor-mode) (text-mode . yas-minor-mode))
 :bind
 (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
 (:map
  yas-keymap
  (("TAB" . smarter-yas-expand-next-field)
   ([(tab)] . smarter-yas-expand-next-field)))
 :config
 (yas-reload-all)
 (defun smarter-yas-expand-next-field ()
   "Try to `yas-expand' then `yas-next-field' at current cursor position."
   (interactive)
   (let ((old-point (point))
         (old-tick (buffer-chars-modified-tick)))
     (yas-expand)
     (when (and (eq old-point (point))
                (eq old-tick (buffer-chars-modified-tick)))
       (ignore-errors
         (yas-next-field))))))

(use-package yasnippet-snippets :defer t :after yasnippet :straight (:build t))

(use-package yatemplate :defer t :after yasnippet :straight (:build t))

(provide 'lolo-completion)
;;; lolo-completion.el ends here
