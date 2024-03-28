;;; init.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(setq-default mode-line-format nil)
(setq auto-mode-case-fold nil)

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "core" "modules"))
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

(require 'lolo-custom)
(require 'lolo-funcs)
(require 'lolo-package)
(require 'lolo-ui)

;; modules
(require 'lolo-which-key)
(require 'lolo-general)
(require 'lolo-hydra)
(require 'lolo-completion)
(require 'lolo-org)
(require 'lolo-misc)
(require 'lolo-programming)
;; (require 'lolo-emacs-lisp)
;; (require 'lolo-python)
;; (require 'lolo-web)
;; (require 'lolo-go)
(require 'lolo-rust)

;;; init.el ends here
(put 'scroll-left 'disabled nil)
