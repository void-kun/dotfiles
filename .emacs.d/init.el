
;;; init.el --- Initialize emacs config.	-*- lexical-binding: t -*-

;; Author: hoangzrik
;; URL: https://github.com/void-kun/dotfiles

;;; Code:

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)
;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))


(message "[LOLO] Lolo is powering up...")

;; define dictionaries structure
(setq load-prefer-newer t)
(defvar lolo-dir (file-name-directory load-file-name))
(defvar lolo-vendor-dir (expand-file-name "vendor" lolo-dir))
(defvar lolo-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Requisites
(require 'lolo-const)
(require 'lolo-custom)
(require 'lolo-funcs)
;; Packages
(require 'lolo-package)
;; Preferences
(require 'lolo-base)
(require 'lolo-hydra)
;; Editor
(require 'lolo-ui)
(require 'lolo-edit)
(require 'lolo-completion)
(require 'lolo-corfu)
(require 'lolo-yasnippet)

(require 'lolo-bookmark)
(require 'lolo-calendar)
(require 'lolo-dashboard)
(require 'lolo-dired)
(require 'lolo-highlight)
(require 'lolo-ibuffer)
(require 'lolo-kill-ring)
(require 'lolo-workspace)
(require 'lolo-window)
(require 'lolo-treemacs)

(require 'lolo-eshell)
(require 'lolo-shell)

(require 'lolo-markdown)
(require 'lolo-org)
(require 'lolo-reader)

(require 'lolo-docker)
(require 'lolo-utils)

;; Programming
(require 'lolo-vcs)
(require 'lolo-flymake)
(require 'lolo-lsp)
(require 'lolo-dap)

(require 'lolo-prog)
(require 'lolo-elisp)
(require 'lolo-c)
(require 'lolo-go)
(require 'lolo-rust)
(require 'lolo-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
