;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; Define custom location variables
(defvar lolo-dir (file-name-directory load-file-name)
    "The root dir of emacs config.")
(defvar lolo-savefile-dir (expand-file-name "savefile" lolo-dir)
    "This folder stores all the automatically generated save/history files.")

;; ============================================================================
;; Function load packages
(defun load-package-dirs (&rest _)
    "Load emacs configuration directories (`site-elisp', `core', `modules')"
    (dolist (dir '("site-elisp" "core" "modules"))
        (push (expand-file-name dir lolo-dir) load-path)))

(defun load-package-subdirs (&rest _)
    "Load emacs package subdirs (`site-elisp')"
    (let ((default-directory (expand-file-name "site-elisp" lolo-dir)))
        (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'load-package-dirs)
(advice-add #'package-initialize :after #'load-package-subdirs)
(load-package-dirs)

;; ============================================================================
;; Load core packages
(require 'lolo-customs)
(require 'lolo-functions)
(require 'lolo-config)
(require 'lolo-packages)
(require 'lolo-keybindings)

;; OS config
(require 'lolo-linux)

;; Beautiful packages
(require 'lolo-builtin)
(require 'lolo-ui)
(require 'lolo-completion)
(require 'lolo-applications)
(require 'lolo-editing)
(require 'lolo-misc)
(require 'lolo-terminal)

;; Programming
(require 'lolo-prog-tool)
(require 'lolo-dsls)
(require 'lolo-c)
(require 'lolo-commonlisp)
(require 'lolo-emacslisp)
(require 'lolo-python)
(require 'lolo-rust)
(require 'lolo-go)
(require 'lolo-web)
(require 'lolo-zig)

;; Load core with packages
(require 'lolo-config-with-packages)

(provide 'init)
;;; init.el ends here
