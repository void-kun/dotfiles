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
(load "/home/zrik/.emacs.d/vendor/go-projectile.el")
(require 'go-projectile)

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

(message "[LOLO] Loading Lolo's additional modules...")

(lolo-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'lolo-tip-of-the-day))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" default))
 '(package-selected-packages
   '(gruber-darker-theme multiple-cursors smex auto-dim-other-buffers go-impl go-fill-struct go-dlv ron-mode yasnippet tree-sitter-langs tree-sitter flycheck-rust cargo rust-mode gotest go-projectile go-mode rainbow-mode elisp-slime-nav rainbow-delimiters counsel swiper ivy lsp-ui lsp-mode company consult orderless vertico zop-to-char monokai-pro-theme which-key volatile-highlights undo-tree super-save smartrep smartparens operate-on-number nlinum move-text magit projectile imenu-anywhere hl-todo guru-mode git-modes git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major crux browse-kill-ring anzu ag ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "IosevkaLyteTerm" :foundry "UKWN" :slant normal :weight regular :height 120 :width normal)))))
