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

(use-package
 counsel
 :straight (:build t)
 :bind
 (("M-x" . counsel-M-x)
  ("C-x b" . counsel-ibuffer)
  ("C-x C-f" . counsel-find-file))
 :config (setq ivy-initial-inputs-alist nil))

(use-package
 ivy
 :straight (:build t)
 :defer t
 :diminish
 :bind
 (("C-s" . swiper)
  :map
  ivy-minibuffer-map
  ("TAB" . ivy-alt-done)
  ("C-l" . ivy-alt-done)
  ("C-t" . ivy-next-line)
  ("C-s" . ivy-previous-line)
  ("C-u" . ivy-scroll-up-command)
  ("C-d" . ivy-scroll-down-command)
  :map
  ivy-switch-buffer-map
  ("C-t" . ivy-next-line)
  ("C-s" . ivy-previous-line)
  ("C-l" . ivy-done)
  ("C-d" . ivy-switch-buffer-kill)
  :map
  ivy-reverse-i-search-map
  ("C-t" . ivy-next-line)
  ("C-s" . ivy-previous-line)
  ("C-d" . ivy-reverse-i-search-kill))
 :config (ivy-mode 1)
 (setq
  ivy-wrap t
  ivy-height 17
  ivy-sort-max-size 50000
  ivy-fixed-height-minibuffer t
  ivy-read-action-functions #'ivy-hydra-read-action
  ivy-read-action-format-function #'ivy-read-action-format-columns
  projectile-completion-system 'ivy
  ivy-on-del-error-function #'ignore
  ivy-initial-inputs-alist nil
  ivy-use-selectable-prompt t))

(use-package ivy-prescient :after ivy :straight (:build t))

(use-package
 all-the-icons-ivy
 :straight (:build t)
 :after (ivy all-the-icons)
 :init (all-the-icons-ivy-setup)
 :hook (after-init . all-the-icons-ivy-setup))
(all-the-icons-ivy-setup)

(use-package
 ivy-posframe
 :defer t
 :after (:any ivy helpful)
 :hook (ivy-mode . ivy-posframe-mode)
 :straight (:build t)
 :init (ivy-posframe-mode 1)
 :config
 (setq
  ivy-fixed-height-minibuffer nil
  ivy-posframe-border-width 10
  ivy-posframe-parameters `((min-width . 90) (min-height . ,ivy-height))))

(use-package ivy-hydra :requires (ivy hydra) :after ivy :straight (:build t))

(use-package ivy-rich :straight (:build t) :after ivy :init (ivy-rich-mode 1))

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
 nerd-icons-completion
 :straight (:build t)
 :hook (vertico-mode . nerd-icons-completion-mode))

(use-package marginalia :hook (after-init . marginalia-mode))

;; ============================================================================
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
