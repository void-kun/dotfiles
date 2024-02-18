;;; init.el

;;; Code:
(defvar lolo-user "zrik")

(message "[LOLO] Lolo is powering up...")

(setq warning-minimum-level :emergency)
(setq warning-minimum-log-level :emergency)
(setq auto-mode-case-fold nil)

;; define dictionaries structure
(setq load-prefer-newer t)
(defvar lolo-dir (file-name-directory load-file-name))
(defvar lolo-core-dir (expand-file-name "core" lolo-dir))
(defvar lolo-modules-dir (expand-file-name "modules" lolo-dir))
(defvar lolo-vendor-dir (expand-file-name "vendor" lolo-dir))
(defvar lolo-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(defvar lolo-personal-dir (expand-file-name "personal" lolo-dir))
;; create savefile folder

(unless (file-exists-p lolo-savefile-dir)
  (make-directory lolo-savefile-dir))

(unless (file-exists-p lolo-vendor-dir)
  (make-directory lolo-vendor-dir))

;; function load subdirectory
(defun lolo-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'"
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (lolo-add-subfolders-to-load-path name)))))

;; load paths
;;(load "/home/zrik/.emacs.d/vendor/go-projectile.el")
;;(require 'go-projectile)

(add-to-list 'load-path lolo-core-dir)
(add-to-list 'load-path lolo-vendor-dir)
(lolo-add-subfolders-to-load-path lolo-vendor-dir)
(add-to-list 'load-path lolo-modules-dir)
(add-to-list 'load-path lolo-personal-dir)

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(message "[LOLO] Loading Lolo's core modules...")

;; load the core stuff
(require 'lolo-packages)
(require 'lolo-custom)
(require 'lolo-ui)
(require 'lolo-core)
(require 'lolo-mode)
(require 'lolo-editor)
(require 'lolo-mappings)
(require 'lolo-modules)

;; load personal modules
(require 'lolo-personal-mappings)

;; Windows specific settings
(when (eq system-type 'windows-nt)
  (require 'lolo-windows))

;; disable warning
(defun fixed-do-after-load-evaluation (abs-file)
  "Disable warning for cl lib."
  (dolist (a-l-element after-load-alist)
    (when (and (stringp (car a-l-element))
               (string-match-p (car a-l-element) abs-file))
      (mapc #'funcall (cdr a-l-element))))
(run-hook-with-args 'after-load-functions abs-file))

(advice-add 'do-after-load-evaluation :override #'fixed-do-after-load-evaluation)
(advice-remove 'do-after-load-evaluation #'fixed-do-after-load-evaluation)

(message "[LOLO] Loading Lolo's additional modules...")

(lolo-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'lolo-tip-of-the-day))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(c-mode zop-to-char yasnippet which-key web-mode vue-mode volatile-highlights vertico undo-tree typescript-mode tree-sitter-langs super-save smartrep smartparens rust-mode ron-mode rainbow-mode rainbow-delimiters orderless operate-on-number nlinum multiple-cursors move-text monokai-pro-theme magit lsp-ui json-mode js2-mode imenu-anywhere hl-todo guru-mode gruber-darker-theme gotest go-projectile git-timemachine git-modes gist flycheck-rust expand-region epl emmet-mode elisp-slime-nav editorconfig easy-kill discover-my-major diminish diff-hl crux counsel consult company clang-format cargo browse-kill-ring auto-dim-other-buffers anzu ag ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "IosevkaLyteTerm" :foundry "UKWN" :slant normal :weight regular :height 120 :width normal)))))
