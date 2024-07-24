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
     (message
      "Emacs ready in %s with %d garbage collections."
      (format "%.2f seconds"
              (float-time
               (time-subtract after-init-time before-init-time)))
      gcs-done)))

;; Define Lolo's directory structure
(defvar lolo-dir (file-name-directory load-file-name)
  "The root dir of emacs config")
(defvar lolo-savefile-dir (expand-file-name "savefile" lolo-dir)
  "This folder stores all the automatically generated save/history-files.")

;; ============================================================================
;; Load modules.
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
(require 'lolo-vars)
(require 'lolo-func)
(require 'lolo-package)
(require 'lolo-linux)

(require 'lolo-core)
(require 'lolo-ui)
(require 'lolo-editor)
(require 'lolo-ibuffer)
(require 'lolo-terminal)
(require 'lolo-git)
(require 'lolo-treemacs)
(require 'lolo-completion)
(require 'lolo-projectile)

(require 'lolo-openai)

;; programming
(require 'lolo-prog)
(require 'lolo-lisp)
(require 'lolo-rust)
(require 'lolo-go)
(require 'lolo-common)
(require 'lolo-web)
(require 'lolo-python)

(require 'lolo-snippet)
(require 'lolo-keybinding)

;;; init.el ends here
