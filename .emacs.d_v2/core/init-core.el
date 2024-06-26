;;; init-core.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; Move around text.
(use-package
 avy
 :defer t
 :bind (("C-z c" . avy-goto-char-timer) ("C-z l" . avy-goto-line))
 :custom (avy-timeout-seconds 0.3) (avy-style 'pre)
 :custom-face
 (avy-lead-face
  ((t (:background "#51afef" :foreground "#870000" :weight bold))))) ;

;; ============================================================================
;; a Collection of Ridiculously Useful eXtensions for Emacs
(use-package
 crux
 :config
 (crux-with-region-or-buffer indent-region)
 (crux-with-region-or-buffer untabify)
 (crux-with-region-or-point-to-eol kill-ring-save)
 (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

;; ============================================================================
;; a generic completion mechanism for Emacs
(use-package
 ivy
 :diminish
 :init
 (use-package amx :defer t)
 (use-package counsel :diminish :config (counsel-mode 1))
 (use-package swiper :defer t)
 (ivy-mode 1)
 :custom
 (ivy-use-virtual-buffers t)
 (ivy-height 20)
 (ivy-on-del-error-function nil)
 (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
 (ivy-count-format " %d/%d  ")
 (ivy-wrap t))

;; ============================================================================
;; color ripgrep
(use-package
 color-rg
 :load-path
 (lambda ()
   (expand-file-name "site-elisp/color-rg" user-emacs-directory)))

;; ============================================================================
;; Winner, a mode to restore previous window layouts
(use-package
 winner
 :ensure nil
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

;; ============================================================================
;; Which Key, a feature that displays the key bindings following the incomplete command.
(use-package
 which-key
 :diminish
 :custom
 (which-key-separator " ")
 (which-key-prefix-prefix "+")
 :config (which-key-mode))

;; ============================================================================
;; Undo tree, a feature that provides a visualization of the undos in a file.
(use-package
 undo-tree
 :defer t
 :diminish undo-tree-mode
 :init (global-undo-tree-mode)
 :custom (undo-tree-visualizer-diff t)
 (undo-tree-history-directory-alist
  `(("." . ,(expand-file-name ".backup" user-emacs-directory))))
 (undo-tree-visualizer-timestamps t))

;; ============================================================================
;; Ace Window, a package for selecting windows to switch to.
(use-package ace-window)

;; ============================================================================
;; History.
(use-package
 recentf
 :ensure nil
 :hook (after-init . recentf-mode)
 :custom
 (recentf-auto-cleanup "05:00am")
 (recentf-max-saved-items 200)
 (recentf-exclude
  '((expand-file-name package-user-dir)
    ".cache"
    ".cask"
    ".elfeed"
    "bookmarks"
    "cache"
    "ido.*"
    "persp-confs"
    "recentf"
    "undo-tree-hist"
    "url"
    "COMMIT_EDITMSG\\'")))

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)

;; Move the backup fies to user-emacs-directory/.backup
(setq backup-directory-alist
      `(("." . ,(expand-file-name ".backup" user-emacs-directory))))

;; Ask before killing emacs
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Automatically kill all active processes when closing Emacs
(setq confirm-kill-processes nil)

;; Turn Off Cursor Alarms
(setq ring-bell-function 'ignore)

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;; Don't Lock Files
(setq-default create-lockfiles nil)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)

;; ad-handle-definition warnings are generated when functions are redefined with `defadvice',
;; they are not helpful.
(setq ad-redefinition-action 'accept)

;; Move Custom-Set-Variables to Different File
(setq custom-file
      (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Enable `erase-buffer' function
(put 'erase-buffer 'disabled nil)

(provide 'init-core)
;;; init-core.el ends here
