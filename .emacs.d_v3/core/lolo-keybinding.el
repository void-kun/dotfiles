;;; lolo-keybinding.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; adjust font size like web browsers
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

(defun lolo/keyboard-quit ()
  "The custom quit."
  (interactive)
  (keyboard-escape-quit)
  (mc/keyboard-quit))

;; quick escape
(global-set-key (kbd "<escape><escape>") #'lolo/keyboard-quit)

;; typing editor
(global-set-key (kbd "C-s-c") 'kill-ring-save)
(global-set-key (kbd "C-s-x") 'kill-region)
(global-set-key (kbd "C-s-v") 'yank)
(global-set-key (kbd "C-k") 'crux-smart-kill-line)
(global-set-key (kbd "C-M-k") 'sp-change-enclosing)
(global-set-key (kbd "C-]") 'sp-select-next-thing-exchange)
(global-set-key (kbd "M-<down>") 'recenter-top-bottom)

;; buffers
(global-set-key (kbd "<f5>") #'revert-buffer-quick)
(global-set-key (kbd "C-x K") 'crux-kill-other-buffers)

;; search
;; (global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-f") 'color-rg-search-input)
(global-set-key (kbd "C-b") 'counsel-buffer-or-recentf)
(global-set-key (kbd "C-s-f") 'query-replace-regexp)

;; windows
(global-set-key (kbd "C-x 4 t") 'crux-transpose-windows)
;; Setup shorcuts for window resize width and height
(global-set-key (kbd "C-z w") #'resize-window-width)
(global-set-key (kbd "C-z h") #'resize-window-height)

;; improved window navigation with ace-window
(global-set-key (kbd "C-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window)

;; move line
(global-set-key (kbd "C-<left>") 'move-beginning-of-line)
(global-set-key (kbd "C-<right>") 'move-end-of-line)
(global-set-key (kbd "M-s-<up>") 'lolo/move-text-up)
(global-set-key (kbd "M-s-<down>") 'lolo/move-text-down)

(global-set-key (kbd "M-s-<return>") 'crux-smart-open-line)
(global-set-key (kbd "C-M-s-<return>") 'crux-smart-open-line-above)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") 'lolo/backward-kill-word)

;; projectile
(global-set-key (kbd "C-x p") 'projectile-command-map)

;; git
(global-set-key (kbd "C-x g") 'magit-status)

;; multiple cursors
(global-set-key (kbd "C-M-s-<up>") 'mc/mmlte--up)
(global-set-key (kbd "C-M-s-<down>") 'mc/mmlte--down)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)

(provide 'lolo-keybinding)
;;; lolo-keybinding.el ends here
