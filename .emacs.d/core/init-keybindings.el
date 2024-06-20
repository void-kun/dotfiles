;;; init-keybindings.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
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

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Indentation help
(global-set-key (kbd "C-^") 'crux-top-join-line)

;; Start proced in a similar manner to dired
(global-set-key (kbd "C-x p") 'proced)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'smex)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)

;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-m") 'discover-my-major)

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; replace zap-to-char functionality with the more powerful zop-to-char
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") 'crux-kill-line-backwards)

(global-set-key [remap kill-whole-line] 'crux-kill-whole-line)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; Magit creates some global keybindings by default
;; but it's a nice to complement them with this one
(global-set-key (kbd "C-c g") 'magit-file-dispatch)

(global-set-key (kbd "C-=") 'er/expand-region)

;; recommended avy keybindings
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

;; additional avy keybindings
(global-set-key (kbd "s-,") 'avy-goto-char)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-c v") 'avy-goto-word-or-subword-1)

;; improved window navigation with ace-window
(global-set-key (kbd "s-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
