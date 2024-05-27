;;; lolo-package.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'lolo-const)
  (require 'lolo-custom)
  (require 'lolo-funcs))

;; At first startup
(when (and (file-exists-p lolo-custom-example-file)
           (not (file-exists-p custom-file)))
  (copy-file lolo-custom-example-file custom-file)

  ;; Test and select the fastest package archives
  (message "Testing connection... Please wait a moment.")
  (set-package-archives (lolo-test-package-archives 'no-chart)))

;; Load `custom-file'
(and (file-readable-p custom-file) (load custom-file))

;; Load custom-post file
(defun load-custom-post-file ()
  "Load custom-post file."
  (cond
   ((file-exists-p lolo-custom-post-file)
    (load lolo-custom-post-file))))
(add-hook 'after-init-hook #'load-custom-post-file)

(defun my-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to option `custom-file'."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add
 'package--save-selected-packages
 :override #'my-package--save-selected-packages)

(set-package-archives lolo-package-archives nil nil t)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil) ; To prevent initializing twice
  (package-initialize))

;; More options
(setq package-install-upgrade-built-in t)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(setq
 use-package-always-ensure t
 use-package-always-defer t
 use-package-expand-minimally t
 use-package-enable-imenu-support t)

;; Required by `use-package'
(use-package diminish :ensure t)


(provide 'lolo-package)
;;; lolo-package.el ends here
