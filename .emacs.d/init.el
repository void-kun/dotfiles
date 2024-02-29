
;;; lolo-init.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(setq-default mode-line-format nil)
(setq auto-mode-case-fold nil)

;; (unless (or (daemonp) noninteractive init-file-debug)
;;   ;; Suppress file handlers operations at startup
;;   ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
;;   (let ((o-value file-name-handler-alist))
;;     (setq file-name-handler-alist nil)
;;     (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
;;     (add-hook 'emacs-startup-hook
;;               (lambda ()
;;                 "Recover file name handlers."
;;                 (setq file-name-handler-alist
;;                       (delete-dups (append file-name-handler-alist o-value))))
;;               101)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "modules" "core"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'.

Don't put large files in `site-lisp' directory, e.g. EAF.
Otherwise the startup will be very slow."
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
;; Without this comment Emacs25 adds (package-initialize) here
(require 'lolo-package)

;; Preferences
(require 'lolo-base)
(require 'lolo-hydra)

(require 'lolo-ui)
(require 'lolo-edit)
(require 'lolo-completion)
(require 'lolo-snippet)

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
(require 'lolo-reader)

;; (require 'lolo-dict)
(require 'lolo-docker)
;; (require 'lolo-player)
(require 'lolo-utils)

;; Programming
(require 'lolo-vcs)
(require 'lolo-check)
(require 'lolo-lsp)
(require 'lolo-dap)

(require 'lolo-prog)
(require 'lolo-elisp)
;; (require 'lolo-c)
;; (require 'lolo-go)
;; (require 'lolo-rust)
;; (require 'lolo-python)
;; (require 'lolo-web)

;;; lolo-init.el ends here
