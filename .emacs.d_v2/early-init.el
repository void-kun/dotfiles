;;; early-init.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(defvar better-gc-cons-threshold 134217728 ; 128*1024*1024 == 128mb
  "The default value to use for `gc-cons-threshold'.")

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq package-enable-at-startup nil)
(setq site-run-file nil)

(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Garbage Collector Optimization Hack
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold better-gc-cons-threshold)
   (setq gc-cons-percentage 0.1)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'file-name-handler-alist-original)))

(add-hook
 'emacs-startup-hook
 (lambda ()
   (if (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function
                     (lambda ()
                       (unless (frame-focus-state)
                         (garbage-collect))))
     (add-hook 'after-focus-change-function 'garbage-collect))
   (defun gc-minibuffer-setup-hook ()
     (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

   (defun gc-minibuffer-exit-hook ()
     (garbage-collect)
     (setq gc-cons-threshold better-gc-cons-threshold))

   (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
   (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

(setq load-prefer-newer noninteractive)
(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq frame-inhibit-implied-resize t)

;; disable warnings
(setq warning-minimum-level :emergency)
(setq byte-compile-warnings '(cl-functions))

;;; early-init.el ends here
