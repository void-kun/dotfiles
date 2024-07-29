;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

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
 ivy
 :straight t
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

;; (use-package ivy-posframe
;;   :defer t
;;   :after (:any ivy helpful)
;;   :hook (ivy-mode . ivy-posframe-mode)
;;   :straight (:build t)
;;   :init
;;   (ivy-posframe-mode 1)
;;   :config
;;   (setq ivy-fixed-height-minibuffer nil
;;         ivy-posframe-border-width   10
;;         ivy-posframe-parameters
;;         `((min-width  . 90)
;;           (min-height . ,ivy-height))))

(use-package ivy-hydra :requires (ivy hydra) :after ivy :straight (:build t))

(use-package ivy-rich :straight (:build t) :after ivy :init (ivy-rich-mode 1))

(use-package
 counsel
 :straight t
 :after recentf
 :after ivy
 :bind
 (("M-x" . counsel-M-x)
  ("C-x b" . counsel-ibuffer)
  ("C-x C-f" . counsel-find-file)
  :map
  minibuffer-local-map
  ("C-r" . 'counsel-minibuffer-history)))

(use-package
 yasnippet
 :defer t
 :straight (:build t)
 :init (yas-global-mode)
 :hook ((prog-mode . yas-minor-mode) (text-mode . yas-minor-mode)))

(use-package yasnippet-snippets :defer t :after yasnippet :straight (:build t))

(use-package yatemplate :defer t :after yasnippet :straight (:build t))

(use-package ivy-yasnippet :defer t :after (ivy yasnippet) :straight (:build t))

(provide 'lolo-autocompletion)
;;; lolo-autocompletion.el ends here
