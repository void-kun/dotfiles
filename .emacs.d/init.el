;;; init.el

;; configuration
(defvar lolo-user "zrik")

(message "[LOLO] Lolo is powering up...")

;; define dictionaries structure
(setq load-prefer-newer t)
(defvar lolo-dir (file-name-directory load-file-name))
(defvar lolo-core-dir (expand-file-name "core" lolo-dir))
(defvar lolo-modules-dir (expand-file-name "modules" lolo-dir))
(defvar lolo-vendor-dir (expand-file-name "vendor" lolo-dir))
(defvar lolo-savefile-dir (expand-file-name "savefile" user-emacs-directory))
;; create savefile folder

(unless (file-exists-p lolo-savefile-dir)
  (make-directory lolo-savefile-dir))

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
(add-to-list 'load-path lolo-core-dir)
(add-to-list 'load-path lolo-modules-dir)
(add-to-list 'load-path lolo-vendor-dir)
(lolo-add-subfolders-to-load-path lolo-vendor-dir)

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

;; Windows specific settings
(when (eq system-type 'windows-nt)
  (require 'lolo-windows))

(message "[LOLO] Loading Lolo's additional modules...")

(lolo-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'lolo-tip-of-the-day))

(custom-set-variables
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-line-numbers-type 'relative)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(yaml-mode elisp-slime-nav rainbow-delimiters lsp-ui lsp-mode company consult orderless vertico counsel swiper ivy zop-to-char zenburn-theme which-key volatile-highlights undo-tree super-save smartrep smartparens projectile operate-on-number nlinum move-text monokai-pro-theme magit imenu-anywhere hl-todo guru-mode git-timemachine git-modes gist flycheck expand-region editorconfig easy-kill discover-my-major diminish diff-hl crux browse-kill-ring anzu ag ace-window))
 '(size-indication-mode t))
