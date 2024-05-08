;;; lolo-base.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:


(require 'subr-x)
(require 'lolo-funcs)

;; Add confirm when kill emacs
(setq confirm-kill-emacs #'y-or-n-p)

;; Compatibility
(use-package compat :demand t)

(with-no-warnings
  ;; Key Modifiers
  (setq command-line-x-option-alist nil)
  ;; Increase how much is read from processes in a single chunk (default is 4kb)
  (setq read-process-output-max #x10000) ; 64kb
  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject))

(use-package
  emacs
  :init

  (setq enable-recursive-minibuffers t)
  (setq backup-by-copying t)
  (setq sentence-end-double-space nil)
  (setq frame-inhibit-implied-resize t)
  (setq show-trailing-whitespace t))

;; (setq whitespace-style
;;       (quote (face spaces tabs newline space-mark tab-mark newline-mark)))

;; (whitespace-mode t)
;; (add-hook 'before-save-hook 'whitespace-cleanup)
;; (add-hook 'prog-mode-hook (lambda () (whitespace-mode t)))

;; (add-hook 'text-mode-hook (lambda () (whitespace-mode t)))

;; (setq whitespace-display-mappings '((space-mark 32 [183] [46])))

;; (set-face-attribute 'whitespace-tab nil :background nil :foreground "#aaaaaa")
;; (set-face-attribute 'whitespace-space nil :background nil :foreground "#aaaaaa")

;; Garbage Collector Magic Hack
(use-package
  gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq
   gcmh-idle-delay 'auto
   gcmh-auto-idle-delay-factor 10
   gcmh-high-cons-threshold #x1000000)) ; 16MB

;; Set UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")
(setq set-selection-coding-system 'utf-8)

;; Environment
(use-package
  exec-path-from-shell
  :custom (exec-path-from-shell-arguments '("-l"))
  :init (exec-path-from-shell-initialize))

;; Start server
(use-package server :if lolo-server :hook (after-init . server-mode))

;; Save place
(use-package saveplace :hook (after-init . save-place-mode))

;; History
(use-package
  recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init
  (setq
   recentf-max-saved-items 300
   recentf-exclude
   '("\\.?cache"
     ".cask"
     "url"
     "COMMIT_EDITMSG\\'"
     "bookmarks"
     "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
     "\\.?ido\\.last$"
     "\\.revive$"
     "/G?TAGS$"
     "/.elfeed/"
     "^/tmp/"
     "^/var/folders/.+$"
     "^/ssh:"
     "/persp-confs/"
     (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package
  savehist
  :hook (after-init . savehist-mode)
  :init
  (setq
   enable-recursive-minibuffers t ; Allow commands in minibuffers
   history-length 1000
   savehist-additional-variables
   '(mark-ring
     global-mark-ring search-ring regexp-search-ring extended-command-history)
   savehist-autosave-interval 300))

;; Misc.
(use-package
  simple
  :ensure nil
  :hook
  ((after-init . size-indication-mode)
   (text-mode . visual-line-mode)
   ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq
   column-number-mode t
   line-number-mode t
   ;; kill-whole-line t               ; Kill line including '\n'
   line-move-visual nil
   track-eol t ; Keep cursor at end of lines. Require line-move-visual is nil.
   set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

  ;; Prettify the process list
  (with-no-warnings
    (defun my-list-processes--prettify ()
      "Prettify process list."
      (when-let ((entries tabulated-list-entries))
        (setq tabulated-list-entries nil)
        (dolist (p (process-list))
          (when-let* ((val (cadr (assoc p entries)))
                      (name (aref val 0))
                      (pid (aref val 1))
                      (status (aref val 2))
                      (status
                       (list
                        status 'face
                        (if (memq status '(stop exit closed failed))
                            'error
                          'success)))
                      (buf-label (aref val 3))
                      (tty (list (aref val 4) 'face 'font-lock-doc-face))
                      (thread (list (aref val 5) 'face 'font-lock-doc-face))
                      (cmd (list (aref val 6) 'face 'completions-annotations)))
            (push (list p (vector name pid status buf-label tty thread cmd))
                  tabulated-list-entries)))))
    (advice-add #'list-processes--refresh :after #'my-list-processes--prettify)))

;; Misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))
(setq-default
 major-mode 'text-mode
 fill-column 80
 tab-width 4
 indent-tabs-mode nil) ; Permanently indent with spaces, never with TABs

(setq
 visible-bell t
 inhibit-compacting-font-caches t ; Don’t compact font caches during GC
 delete-by-moving-to-trash t ; Deleting files go to OS's trash folder
 make-backup-files nil ; Forbide to make backup files
 auto-save-default nil ; Disable auto save

 uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
 adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
 adaptive-fill-first-line-regexp "^* *$"
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil
 word-wrap-by-category t)

;; Frame
(when (display-graphic-p)
  ;; Frame fullscreen
  (bind-key "S-s-<return>" #'toggle-frame-fullscreen)

  ;; Resize and re-position frames conveniently
  ;; Same keybindings as Rectangle on macOS
  (bind-keys
   ("C-M-<return>" . lolo-frame-maximize)
   ("C-M-<backspace>" . lolo-frame-restore)
   ("C-M-<left>" . lolo-frame-left-half)
   ("C-M-<right>" . lolo-frame-right-half)
   ("C-M-<up>" . lolo-frame-top-half)
   ("C-M-<down>" . lolo-frame-bottom-half))

  ;; Frame transparence
  (use-package
    transwin
    :bind
    (("C-M-9" . transwin-inc)
     ("C-M-8" . transwin-dec)
     ("C-M-7" . transwin-toggle))
    :init
    (when sys/linux-x-p
      (setq transwin-parameter-alpha 'alpha-background))))

;; Global keybindings
(bind-keys
 ("s-r" . revert-this-buffer)
 ("C-x K" . delete-this-file)
 ("C-c C-l" . reload-init-file))

;; Sqlite
(when (fboundp 'sqlite-open)
  (use-package emacsql-sqlite-builtin))

(provide 'lolo-base)
;;; lolo-base.el ends here
