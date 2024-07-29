;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'lolo-customs))

;; ============================================================================
;; Config package.
(setq package-user-dir (expand-file-name "elpa" lolo-dir))
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Straight
(defvar bootstrap-version)
(defvar comp-deferred-compilation-deny-list ()) ; workaround, otherwise straight shits itself
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" lolo-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Initialize package contents
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(straight-use-package '(use-package :build t))
(setq use-package-always-ensure t)

(provide 'lolo-packages)
;;; lolo-packages.el ends here
