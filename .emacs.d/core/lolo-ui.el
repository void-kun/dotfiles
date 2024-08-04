;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package
 dashboard
 :after all-the-icons
 :config
 (setq
  dashboard-banner-logo-title "Zrik Vanilla Emacs"
  dashboard-startup-banner 'logo
  dashboard-center-content t
  dashboard-show-shortcuts t
  dashboard-set-navigator t
  dashboard-set-heading-icons t
  dashboard-set-file-icons t
  initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
  dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

 (setq dashboard-items '((recents . 15) (agenda . 10) (projects . 10)))
 (dashboard-setup-startup-hook)
 :init (add-hook 'after-init-hook 'dashboard-refresh-buffer))

;; ============================================================================
(use-package all-the-icons :defer t)

(defun prog-mode-set-symbols-alist ()
  (setq prettify-symbols-alist '(("lambda" . ?λ)))
  (prettify-symbols-mode 1))

(add-hook 'prog-mode-hook #'prog-mode-set-symbols-alist)

;; ============================================================================
;; (use-package
;;  modus-themes
;;  :defer t
;;  :config
;;  (setq
;;   modus-themes-custom-auto-reload nil
;;   modus-themes-mixed-fonts t
;;   modus-themes-variable-pitch-ui t
;;   modus-themes-italic-constructs t
;;   modus-themes-bold-constructs nil
;;   modus-themes-completions '((t . (extrabold)))
;;   modus-themes-prompts '(extrabold)
;;   modus-themes-headings
;;   '((agenda-structure . (variable-pitch light 2.2))
;;     (agenda-date . (variable-pitch regular 1.3))
;;     (t . (regular 1.15))))
;;  (setq modus-themes-common-palette-overrides nil))
;; (load-theme 'modus-operandi-tritanopia :no-confirm)

(use-package gruvbox-theme
  :defer t)

(load-theme 'gruvbox-dark-medium :no-confirm)

;; ============================================================================
(use-package
 doom-modeline
 :defer t
 :init
 (doom-modeline-mode 1)
 (setq find-file-visit-truename t)
 :custom
 (doom-modeline-support-imenu t)
 (doom-modeline-bar-width 4)
 (doom-modeline-hud nil)
 (doom-modeline-window-width-limit 85)
 (doom-modeline-project-detection 'auto)
 (doom-modeline-height 40)
 (doom-modeline-enable-word-count t)
 (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
 (doom-modeline-env-version t)
 (doom-modeline-buffer-file-name-style 'truncate-upto-project)
 (inhibit-compacting-font-caches t)
 (find-file-visit-truename t)
 :config
 (custom-set-faces
  '(mode-line ((t (:family "IosevkaLyteTerm" :height 1.0))))
  '(mode-line-active
    ((t (:family "IosevkaLyteTerm" :height 1.0)))) ; For 29+
  '(mode-line-inactive ((t (:family "IosevkaLyteTerm" :height 1.0))))))

;; ============================================================================
(use-package
 winner
 :custom
 (winner-boring-buffers
  '("*Completions*"
    "*Compile-Log*"
    "*inferior-lisp*"
    "*Fuzzy Completions*"
    "*Apropos*"
    "*Help*"
    "*cvs*"
    "*Buffer List*"
    "*Ibuffer*"
    "*esh command on file*"))
 :config (winner-mode 1))

;;; Directional window motions (windmove)
(use-package
 windmove
 :bind
 (("C-M-s-<up>" . windmove-up)
  ("C-M-s-<right>" . windmove-right)
  ("C-M-s-<down>" . windmove-down)
  ("C-M-s-<left>" . windmove-left)
  ("C-M-S-<up>" . windmove-swap-states-up)
  ("C-M-S-<right>" . windmove-swap-states-right) ; conflicts with `org-increase-number-at-point'
  ("C-M-S-<down>" . windmove-swap-states-down)
  ("C-M-S-<left>" . windmove-swap-states-left))
 :config
 (setq windmove-create-window nil)) ; Emacs 27.1

;;; Header line context of symbol/heading (breadcrumb.el)
(use-package
 breadcrumb
 :functions (breadcrumb-local-mode)
 :hook ((text-mode prog-mode) . lolo/breadcrumb-local-mode)
 :config
 (setq breadcrumb-project-max-length 0.5)
 (setq breadcrumb-project-crumb-separator "/")
 (setq breadcrumb-imenu-max-length 1.0)
 (setq breadcrumb-imenu-crumb-separator " > ")

 (defun lolo/breadcrumb-local-mode ()
   "Enable `breadcrumb-local-mode' if the buffer is visiting a file."
   (when buffer-file-name
     (breadcrumb-local-mode 1))))

(provide 'lolo-ui)
;;; lolo-ui.el ends here
