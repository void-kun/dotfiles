;;; early-init.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(defvar better-gc-cons-threshold 134217728 ; 128*1024*1024 == 128mb
  "The default value to use for `gc-cons-threshold'.")

(setq package-enable-at-startup nil)
(setq site-run-file nil)

;; increate process output to 1mb intead 4kb
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; disable menu-bar and scroll-bar
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; garbage collector optimization
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold better-gc-cons-threshold)
   (setq gc-cons-percentage 0.1)))

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
(setq-default mode-line-format nil)

;; disable warnings
;; (setq warning-minimum-level :emergency)
;; (setq byte-compile-warnings '(cl-functions))

;;; early-init.el ends here
