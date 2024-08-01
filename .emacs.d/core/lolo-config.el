;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'lolo-functions))

;; ============================================================================
;; Unbind unneeded keys.
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
(global-set-key (kbd "C-d") nil)
(global-set-key (kbd "M-<backspace>") nil)

;; Create savefile folder if not exists
(unless (file-exists-p lolo-savefile-dir)
  (make-directory lolo-savefile-dir))

;; Move the backup fies to user-emacs-directory/savefile/.backup
(setq backup-directory-alist
      `((".*" . ,(expand-file-name ".backup" lolo-savefile-dir))))

;; enable y/n answers
(setopt use-short-answers t)

;; Ask before killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

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

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Enable `erase-buffer' function
(put 'erase-buffer 'disabled nil)

;; Nice scrolling
(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; Delete the selection with a keypress
(delete-selection-mode t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; ============================================================================
;; Editing text.
(add-hook 'before-save-hook #'whitespace-cleanup)

(setq-default sentence-end-double-space nil)

(global-subword-mode 1)

(setq scroll-conservatively 1000)

;; ============================================================================
;; Indentation.
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; ============================================================================
;; GPG pinentry.
(setq epg-pinentry-mode 'loopback)

;; ============================================================================
;; Line Number.
(global-display-line-numbers-mode t)
(global-hl-line-mode t)

;; ============================================================================
;; Load custom file.
(setq-default custom-file (expand-file-name ".custom.el" lolo-dir))

(setq delete-by-moving-to-trash t)

;; ============================================================================
;; Misc.
(setq
 undo-limit 100000000
 auto-save-default nil)

(setq window-combination-resize t) ; take new window space from all other windows

;; ============================================================================
;; Visual Configuration.
(setq visible-bell t)
(setq x-stretch-cursor t)
(with-eval-after-load 'mule-util
  (setq truncate-string-ellipsis "…"))
;; (add-to-list 'default-frame-alist '(alpha-background . 0.9))

;; ============================================================================
;; Modeline Modules.
(require 'time)
(setq display-time-format "%Y-%m-%d %H:%M")
(display-time-mode 1) ; display time in modeline
(column-number-mode)
(defun modeline-contitional-buffer-encoding ()
  "Hide \"LF UTF-8\" in modeline.

It is expected of files to be encoded with LF UTF-8, so only show
the encoding in the modeline if the encoding is worth notifying
the user."
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq
                            (plist-get
                             (coding-system-plist buffer-file-coding-system)
                             :category)
                            '(coding-category-undecided coding-category-utf-8))
                           (not
                            (memq
                             (coding-system-eol-type
                              buffer-file-coding-system)
                             '(1 2))))
                t)))
(add-hook 'after-change-major-mode-hook #'modeline-contitional-buffer-encoding)

;; ============================================================================
;; Fonts.
(lolo/set-font)
(add-hook 'server-after-make-frame-hook #'lolo/set-font)

(provide 'lolo-config)
;;; lolo-config.el ends here
