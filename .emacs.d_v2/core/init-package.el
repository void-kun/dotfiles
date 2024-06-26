;;; init-package.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; Config package.
(setq
 package-user-dir (expand-file-name "elpa" user-emacs-directory)
 package-archives
 '(("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
   ("cselpa" . "https://elpa.thecybershadow.net/packages/")))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil) ; To prevent initializing twice
  (package-initialize))

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose
        (not (bound-and-true-p byte-compile-current-file))))

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

;; ============================================================================
;; Base packages.
(use-package
 auto-package-update
 :if (not (daemonp))
 :custom
 (auto-package-update-interval 7) ;; in days
 (auto-package-update-prompt-before-update t)
 (auto-package-update-delete-old-versions t)
 (auto-package-update-hide-results t)
 :config (auto-package-update-maybe))

(use-package diminish)

(provide 'init-package)
;;; init-package.el ends here
