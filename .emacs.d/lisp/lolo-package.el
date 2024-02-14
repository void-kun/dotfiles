
;;; lolo-package.el --- xxx.	-*- lexical-binding: t -*-

;; Author: hoangzrik
;; URL: https://github.com/void-kun/dotfiles

;;; Code:

(eval-when-compile
  (require 'lolo-const)
  (require 'lolo-custom)
  (require 'lolo-funcs))

;; Load `lolo-custom-file'
(and (file-readable-p lolo-custom-file) (load lolo-custom-file))

;; Load custom-post file
(defun load-post-file ()
  "Load custom-post file."
  (cond ((file-exists-p lolo-post-org-file)
         (and (fboundp 'org-babel-load-file)
              (org-babel-load-file lolo-post-org-file)))
        ((file-exists-p lolo-post-file)
         (load lolo-post-file))))
(add-hook 'after-init-hook #'load-post-file)

(defun my-package--save-selected-packages (&optional value)
    "Set `package-selected-packages' to VALUE but don't save to option `custom-file'."
    (when value
        (setq package-selected-packages value))
    (unless after-init-time
        (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)

;; Initialize packages
(setq package-enable-at-startup nil)          ; To prevent initializing twice
(package-initialize)

;; More options
(setq package-install-upgrade-built-in t)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

;; Should set before loading `use-package'
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; Required by `use-package'
(use-package diminish :ensure t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Update packages
(unless (fboundp 'package-upgrade-all)
        (use-package auto-package-update
            :init
            (setq auto-package-update-delete-old-versions t
                  auto-package-update-hide-results t)
            (defalias 'package-upgrade-all #'auto-package-update-now)))

(provide 'lolo-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lolo-package.el ends here
