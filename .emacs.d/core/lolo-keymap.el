;;; lolo-keymap.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; main config
(global-set-key (kbd "<escape><escape>") 'keyboard-escape-quit)

;; remap copy/paste
(global-set-key (kbd "C-s-c") 'kill-ring-save)
(global-set-key (kbd "C-s-x") 'kill-region)
(global-set-key (kbd "C-s-v") 'yank)
(global-set-key (kbd "C-k") 'lolo/kill-line)

;; remap move line
(global-set-key (kbd "M-s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "M-s-<right>") 'move-end-of-line)
(global-set-key (kbd "M-s-<up>") 'lolo/move-text-up)
(global-set-key (kbd "M-s-<down>") 'lolo/move-text-down)

(global-set-key (kbd "M-s-<return>") 'lolo/newline-with-indent-below)
(global-set-key (kbd "C-M-s-<return>") 'lolo/newline-with-indent-above)

;; remap windows
(global-set-key (kbd "C-w") nil)
(global-set-key (kbd "C-w C-w") 'other-window)
(global-set-key (kbd "C-w C-h") 'split-window-horizontally)
(global-set-key (kbd "C-w C-v") 'split-window-vertically)
(global-set-key (kbd "C-w C-q") 'delete-window)


(provide 'lolo-keymap)
;;; lolo-keymap.el ends here
