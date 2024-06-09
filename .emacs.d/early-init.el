;;; early-init.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(when (or (daemonp)
          noninteractive)
  (setq package-enable-at-startup nil))

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; Garbage Collector Optimization Hack
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024) ; 16mb
                  gc-cons-percentage 0.1)))


;; The command-line option ‘-batch’ makes Emacs to run noninteractively.
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code.  Otherwise, skipping the mtime checks
;; on every *.elc file saves a bit of IO time.
(setq load-prefer-newer noninteractive)

;; Contrary to common configurations, this is all that's needed to set UTF-8
;; as the default coding system:
(set-language-environment "UTF-8")

;; `set-language-enviornment' sets `default-input-method', which is unwanted.
(setq default-input-method nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq tool-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Currently I use menubar on graphical mode.
(when (and (not (display-graphic-p)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font.  By inhibiting this, the startup time is significantly reduced,
;; especially with fonts larger than the system default.
(setq frame-inhibit-implied-resize t)

;; disable warnings
(setq warning-minimum-level :emergency)

(provide 'early-init)
;;; early-init.el ends here
