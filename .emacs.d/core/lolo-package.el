;;; lolo-package.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(require 'package)

(eval-when-compile
  (require 'lolo-custom)
  (require 'lolo-funcs))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(setq package-enable-at-startup nil)

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package gcmh
  :diminish gcmh-mode
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  (gcmh-mode 1))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-percentage 0.1))) ;; Default value for `gc-cons-percentage'

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(load custom-file)

;; ============================================================================

(require 'on)

(setq register-preview-delay 0) ;; Show registers ASAP

(set-register ?i (cons 'file (concat org-directory   "/cpb.org")))
(set-register ?h (cons 'file (concat org-directory   "/work.org")))
(set-register ?C (cons 'file (concat lolo/emacs-stuff "/init.org")))
(set-register ?A (cons 'file (concat org-directory   "/org-archive/homework-archive.org_archive")))
(set-register ?T (cons 'file (concat org-directory   "/org-archive/todo-archive.org_archive")))

(server-start)
;; A cool mode to revert window configurations.
(winner-mode 1)

;; INTERACTION -----
(setq use-short-answers t) ;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(setq confirm-kill-emacs 'yes-or-no-p) ;; Confirm to quit
(setq initial-scratch-message ""
      initial-buffer-choice t) ;; Blank scratch buffer

;; WINDOW -----------
(setq frame-resize-pixelwise t)
(setq ns-pop-up-frames nil) ;; When opening a file (like double click) on Mac, use an existing frame
(setq window-resize-pixelwise nil)
(setq split-width-threshold 80) ;; How thin the window should be to stop splitting vertically (I think)

;; LINES -----------
(setq-default truncate-lines t)
(setq-default tab-width 4)
(setq-default fill-column 80)
(setq line-move-visual t) ;; C-p, C-n, etc uses visual lines

(use-package paren
  ;; highlight matching delimiters
  :ensure nil
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode 1))

(setq sentence-end-double-space nil) ;; Sentences end with one space

(setq bookmark-set-fringe-mark nil)

;; SCROLLING ---------
(setq scroll-conservatively 101)
(setq
 mouse-wheel-follow-mouse 't
 mouse-wheel-progressive-speed nil
 ;; The most important setting of all! Make each scroll-event move 2 lines at
 ;; a time (instead of 5 at default). Simply hold down shift to move twice as
 ;; fast, or hold down control to move 3x as fast. Perfect for trackpads.
 mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . 6)))
(setq mac-redisplay-dont-reset-vscroll t ;; sane trackpad/mouse scroll settings (doom)
      mac-mouse-wheel-smooth-scroll nil)

;; BELL/WARNING ------------
(setq visible-bell nil) ;; Make it ring (so no visible bell) (default)
(setq ring-bell-function 'ignore) ;; BUT ignore it, so we see and hear nothing

;; Uses system trash rather than deleting forever
(setq trash-directory (concat lolo/home ".Trash"))
(setq delete-by-moving-to-trash t)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; When opening a symlink that links to a file in a git repo, edit the file in the
;; git repo so we can use the Emacs vc features (like Diff) in the future
(setq vc-follow-symlinks t)

;; BACKUPS/LOCKFILES --------
;; Don't generate backups or lockfiles.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/"))))

(use-package recentf
  :ensure nil
  :config
  (setq ;;recentf-auto-cleanup 'never
   ;; recentf-max-menu-items 0
   recentf-max-saved-items 200)
  (setq recentf-filename-handlers ;; Show home folder path as a ~
        (append '(abbreviate-file-name) recentf-filename-handlers))
  (recentf-mode))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; ENCODING -------------
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please

(setq default-input-method "spanish-postfix") ;; When I need to type in Spanish (switch with C-\)

(setq blink-cursor-interval 0.6)
(blink-cursor-mode 0)

(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(setq what-cursor-show-names t) ;; improves C-x =

(setq dired-kill-when-opening-new-dired-buffer t)

(setq reb-re-syntax 'string)

;; ===================================================================
(provide 'lolo-package)
;;; lolo-package.el ends here
