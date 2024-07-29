;;; -*- lexical-binding: t; -*-
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
;; Manage keybindings.
(use-package general :straight (:build t) :init (general-auto-unbind-keys))

;; ============================================================================
;; Menu creator for keybindings.
(use-package hydra :straight (:build t) :defer t)

(general-define-key

 ;; quick quit
 "<escape>"
 'keyboard-escape-quit

 ;; typing editor
 "C-S-c"
 'kill-ring-save
 "C-S-x"
 'kill-region
 "C-S-v"
 'yank
 "C-k"
 'crux-smart-kill-line
 "C-M-k"
 'sp-change-enclosing
 "C-]"
 'sp-select-next-thing-exchange
 "M-<down>"
 'recenter-top-bottom

 ;; adjust font size
 "C-="
 'acg/zoom-frame
 "C--"
 'acg/zoom-frame-out

 ;; mark all
 "C-a"
 'mark-whole-buffer
 "C-x K"
 'crux-kill-other-buffers

 ;; search
 "C-f"
 'color-rg-search-input
 "C-b"
 'counsel-buffer-or-recentf
 "C-s-f"
 'query-replace-regexp

 ;; move line
 "C-<left>" 'move-beginning-of-line
 "C-<right>" 'move-end-of-line
 "M-s-<up>" 'lolo/move-text-up
 "M-s-<down>" 'lolo/move-text-down
 "C-<backspace>" 'lolo/backward-kill-word
 "C-x g" 'magit-status)

;; projectile
(global-set-key (kbd "C-x p") 'projectile-command-map)

(global-set-key (kbd "C-w") #'lolo/switch-window)
(global-set-key [remap other-window] 'ace-window)

(provide 'lolo-keybinding)
;;; lolo-keybinding.el ends here
