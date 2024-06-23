;;; init.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; Profiling and Debug.
(defconst emacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, Emacs will be verbose.
Set DEBUG=1 in the command line or use --debug-init to enable this.")

;; Set the `debug-on-error' variable as per the runtime context:
;; - Enable debugging on error if Emacs is running in interactive mode, and the
;;   custom variable `emacs-debug-mode' is true.
;; - Do not enable debugging on error in non-interactive mode, regardless of the
;;   `emacs-debug-mode' value.
(setq-default debug-on-error (and (not noninteractive) emacs-debug-mode))

;; ============================================================================
;; Measure the current start up time.
(add-hook
 'emacs-startup-hook
 #'(lambda ()
     (message "Emacs ready in %s with %d garbage collections."
              (format "%.2f seconds"
                      (float-time
                       (time-subtract after-init-time before-init-time)))
              gcs-done)))

;; Define Lolo's directory structure
(defvar lolo-dir (file-name-directory load-file-name)
  "The root dir of emacs config")
(defvar lolo-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

;; ============================================================================
;; Load modules.
(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((o-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value
     'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist o-value))))
              101)))

;; Load path
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "core" "modules"))
    (push (expand-file-name dir lolo-dir) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" lolo-dir)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Required
(require 'init-custom)
(require 'init-funs)
(require 'init-packages)
(require 'init-lolo-mode)
(require 'init-core)
(require 'init-ui)

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'init-linux))

;; Modules
(require 'init-vertico)
(require 'init-company)

(require 'init-lsp)

(require 'init-c)
(require 'init-css)
(require 'init-emacs-lisp)
(require 'init-js)
(require 'init-lisp)
(require 'init-perl)
(require 'init-shell)
(require 'init-web)
(require 'init-xml)
(require 'init-yaml)
(require 'init-python)
(require 'init-rust)
(require 'init-go)

(require 'init-keybindings)
;; Load `custom-file'
(and (file-readable-p custom-file) (load custom-file))

(provide 'init)
;;; init.el ends here
