;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; Display the keys.
(use-package
 which-key
 :straight (:build t)
 :defer t
 :init (which-key-mode)
 :diminish which-key-mode
 :config (setq which-key-idle-delay 1))

;; ============================================================================
;; Menu creator for keybindings.
(use-package hydra :straight (:build t) :defer t)

;; ============================================================================
;; Manage keybindings.

;; quick quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; typing editor
(global-set-key (kbd "C-S-c") 'kill-ring-save)
(global-set-key (kbd "C-S-x") 'kill-region)
(global-set-key (kbd "C-S-v") 'yank)
(global-set-key (kbd "C-k") 'crux-smart-kill-line)
(global-set-key (kbd "C-M-k") 'sp-change-enclosing)
(global-set-key (kbd "C-]") 'sp-select-next-thing-exchange)
(global-set-key (kbd "M-<down>") 'recenter-top-bottom)
(global-set-key (kbd "M-s-<return>") 'crux-smart-open-line)
;; adjust font size
(global-set-key (kbd "C-=") 'lolo/zoom-in)
(global-set-key (kbd "C--") 'lolo/zoom-out)
;; mark all
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-x K") 'crux-kill-other-buffers)
;; search
(global-set-key (kbd "C-f") 'color-rg-search-input)
(global-set-key (kbd "C-b") 'counsel-buffer-or-recentf)
(global-set-key (kbd "C-s-f") 'query-replace-regexp)
;; move line
(global-set-key (kbd "C-<left>") 'move-beginning-of-line)
(global-set-key (kbd "C-<right>") 'move-end-of-line)
(global-set-key (kbd "M-s-<up>") 'lolo/move-text-up)
(global-set-key (kbd "M-s-<down>") 'lolo/move-text-down)
(global-set-key (kbd "C-<backspace>") 'lolo/backward-kill-word)
(global-set-key (kbd "C-x g") 'magit-status)
;; projects
(global-set-key (kbd "C-x C-p") 'counsel-projectile-switch-project)
;; improved window navigation with ace-window
(global-set-key (kbd "C-w") #'lolo/switch-window)
(global-set-key [remap other-window] 'ace-window)

(provide 'lolo-keybindings)
;;; lolo-keybindings.el ends here
