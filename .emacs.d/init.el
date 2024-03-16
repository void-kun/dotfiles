;;; init.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq warning-minimum-level :emergency)
(setq gc-cons-threshold most-positive-fixnum)
(setq auto-mode-case-fold nil)
(setq load-prefer-newer t)
(setq-default mode-line-format nil)

(defvar lolo-user
  (getenv "USER"))

(message "[Lolo] start...")

(defvar lolo-dir (file-name-directory load-file-name)
  "The root emacs folder")
(defvar lolo-personal-dir (expand-file-name "personal" lolo-dir)
  "The personal folder")
(defvar lolo-core-dir (expand-file-name "core" lolo-dir)
  "The core folder")
(defvar lolo-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "The savefile folder")
(defvar lolo-modules-file (expand-file-name "lolo-modules.el" lolo-personal-dir)
  "This file contains a list of modules that will be loaded by lolo.")

;; Load path
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "modules" "core"))
    (push (expand-file-name dir lolo-dir) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)
(update-load-path)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Core
(require 'lolo-const)
;; (require 'lolo-funcs)
(require 'lolo-packages)
(require 'lolo-ui)
(require 'lolo-core)
(require 'lolo-mode)
(require 'lolo-editor)
(require 'lolo-global-keybindings)

(require 'lolo-linux)


;; the modules
(if (file-exists-p lolo-modules-file)
    (load lolo-modules-file)
  (message "[lolo] Missing personal modules file %s" lolo-modules-file)
  (message "[lolo] Falling back to the bundled example file sample/lolo-modules.el")
  (message "[lolo] You should copy this file to your personal configuration folder and tweak it to your liking"))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p lolo-personal-dir)
  (message "[Lolo] Loading personal configuration files in %s..." lolo-personal-dir)
  (mapc 'load (delete
               lolo-modules-file
               (directory-files lolo-personal-dir 't "^[^#\.].*\\.el$"))))

(lolo-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'lolo-tip-of-the-day))

;;; init.el ends here
