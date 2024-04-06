;;; init.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'lolo-benchmark) ;; Measure startup time

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'lolo-utils)
(require 'lolo-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'lolo-elpa)      ;; Machinery for installing required packages
(require 'lolo-exec-path) ;; Set up $PATH

(when (require-package 'gcmh)
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode)
                               (diminish 'gcmh-mode))))

(setq jit-lock-defer-time 0)

;; Allow users to provide an optional "lolo-preload-local.el"
(require 'lolo-preload-local nil t)

;; Load configs for specific features and modes
(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(require 'lolo-frame-hooks)
(require 'lolo-xterm)
(require 'lolo-themes)
(require 'lolo-modeline)

(require 'lolo-keys)
(require 'lolo-gui-frames)
(require 'lolo-dired)

(require 'lolo-isearch)
(require 'lolo-grep)
(require 'lolo-uniquify)
(require 'lolo-ibuffer)
(require 'lolo-flymake)
(require 'lolo-eglot)

(require 'lolo-recentf)
(require 'lolo-minibuffer)
(require 'lolo-hippie-expand)
(require 'lolo-corfu)
(require 'lolo-windows)
;; (require 'lolo-sessions)
(require 'lolo-mmm)

(require 'lolo-editing-utils)
(require 'lolo-whitespace)

(require 'lolo-vc)
(require 'lolo-darcs)
(require 'lolo-git)
(require 'lolo-github)

(require 'lolo-projectile)

(require 'lolo-compile)
(require 'lolo-crontab)
(require 'lolo-textile)

;; programming
(require 'lolo-markdown)
(require 'lolo-csv)
(require 'lolo-erlang)
(require 'lolo-javascript)

(require 'lolo-org)
(require 'lolo-nxml)
(require 'lolo-html)
(require 'lolo-css)
(require 'lolo-haml)
(require 'lolo-http)
(require 'lolo-python)

(require 'lolo-sql)
(require 'lolo-rust)
(require 'lolo-toml)
(require 'lolo-yaml)
(require 'lolo-docker)
(require 'lolo-terraform)
(require 'lolo-nix)

(maybe-require-package 'nginx-mode)
(maybe-require-package 'just-mode)
(maybe-require-package 'justl)

(require 'lolo-paredit)
(require 'lolo-lisp)
(require 'lolo-sly)
(require 'lolo-clojure)
(require 'lolo-clojure-cider)

(when *spell-check-support-enabled*
  (require 'lolo-spelling))

(require 'lolo-misc)
(require 'lolo-folding)
(require 'lolo-terminals)

(require-package 'sudo-edit)
(require-package 'gnuplot)
(require-package 'htmlize)
(maybe-require-package 'dotenv-mode)
(maybe-require-package 'shfmt)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(require 'lolo-direnv)

(when (and (require 'treesit nil t)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'lolo-treesitter))

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode
;;                                 "\\.go\\'" . go-mode))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; Locales (setting them earlier in this file doesn't work in X)
(require 'lolo-locales)

;;; init.el ends here
