;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package transient :defer t)

(use-package hl-todo :defer t :init (global-hl-todo-mode 1))

(use-package
 projectile
 :diminish projectile-mode
 :init (setq projectile-switch-project-action #'projectile-switch-project)
 :config
 (projectile-mode 1)
 (add-to-list 'projectile-globally-ignored-directories "node_modules")
 (setq projectile-track-known-projects-automatically nil))

;; ============================================================================
(use-package
 wttrin
 :defer t
 :config
 (setq
  wttrin-default-cities '("Ho Chi Minh City")
  wttrin-use-metric t))

(provide 'lolo-applications)
;;; lolo-applications.el ends here
