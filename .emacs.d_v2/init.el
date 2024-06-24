;;; init.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

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
  (dolist (dir '("site-elisp" "core" "modules"))
    (push (expand-file-name dir lolo-dir) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-elisp" lolo-dir)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Required
(require 'init-custom)
(require 'init-funs)
(require 'init-package)
(require 'init-ui)
(require 'init-core)
(require 'init-terminal)
(require 'init-ibuffer)
(require 'init-git)
(require 'init-projectile)
(require 'init-snippet)
(require 'init-editor)

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'init-linux))

;; Programmings
(require 'init-lsp)
(require 'init-company)
(require 'init-cpp)
(require 'init-go)
(require 'init-python)
(require 'init-rust)
(require 'init-web)
(require 'init-yaml)

(require 'init-keybindings)

;;; init.el ends here
