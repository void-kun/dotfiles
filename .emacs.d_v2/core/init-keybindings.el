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

;; Quick escape quit 
(global-set-key (kbd "<escape><escape>") 'keyboard-escape-quit)

;; remap copy/paste
(global-set-key (kbd "C-s-c") 'kill-ring-save)
(global-set-key (kbd "C-s-x") 'kill-region)
(global-set-key (kbd "C-s-v") 'yank)
(global-set-key (kbd "C-k") 'crux-smart-kill-line)

;; improved window navigation with ace-window
(global-set-key (kbd "C-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
