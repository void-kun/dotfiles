;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;; Code:

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

;; Core packages
(require 'lolo-core)
(require 'lolo-customs)
(require 'lolo-functions)
(require 'lolo-packages)
(require 'lolo-keybinding)

;; OS config
(require 'lolo-linux)

;; Beatiful packages
(require 'lolo-autocompletion)
(require 'lolo-editing)
(require 'lolo-builtin)
(require 'lolo-misc)
(require 'lolo-application)
(require 'lolo-ui)

;; ;; Programming
;; (require 'lolo-prog-tool)
;; (require 'lolo-dsls)
;; (require 'lolo-c)
;; (require 'lolo-commonlisp)
;; (require 'lolo-emacslisp)
;; (require 'lolo-python)
;; (require 'lolo-rust)
;; (require 'lolo-go)
;; (require 'lolo-web)
;; (require 'lolo-zig)

;; Load the custom file
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
