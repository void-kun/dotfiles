;;; init-keybindings.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)

;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

;; Revert buffer
(global-set-key (kbd "<f5>") #'revert-buffer-quick)

;; Quick escape
(global-set-key (kbd "<escape><escape>") 'keyboard-escape-quit)

;; remap copy/paste
(global-set-key (kbd "C-s-c") 'kill-ring-save)
(global-set-key (kbd "C-s-x") 'kill-region)
(global-set-key (kbd "C-s-v") 'yank)
(global-set-key (kbd "C-k") 'crux-smart-kill-line)

;; improved window navigation with ace-window
(global-set-key (kbd "C-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window)

;; remap move line
(global-set-key (kbd "M-s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "M-s-<right>") 'move-end-of-line)
(global-set-key (kbd "M-s-<up>") 'lolo/move-text-up)
(global-set-key (kbd "M-s-<down>") 'lolo/move-text-down)

(global-set-key (kbd "M-s-<return>") 'crux-smart-open-line)
(global-set-key (kbd "C-M-s-<return>") 'crux-smart-open-line-above)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") 'crux-kill-line-backwards)

(global-set-key [remap kill-whole-line] 'crux-kill-whole-line)

;; terminal
(global-set-key (kbd "C-<escape>") 'vterm-toggle)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
