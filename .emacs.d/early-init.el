;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(setq
 frame-resize-pixelwise t
 frame-inhibit-implied-resize t
 frame-title-format '("%b")
 ring-bell-function 'ignore

 package-native-compile t
 package-enable-at-startup t

 use-dialog-box t ; only for mouse events, which I seldom use
 use-file-dialog nil
 use-short-answers t

 inhibit-splash-screen t
 inhibit-startup-screen t
 inhibit-x-resources t
 inhibit-startup-echo-area-message user-login-name ; read the docstring
 inhibit-startup-buffer-menu t)

(scroll-bar-mode -1) ; disable scrollbar
(tool-bar-mode -1) ; disable toolbar
(tooltip-mode -1) ; disable tooltips
(set-fringe-mode 10) ; give some breathing room
(menu-bar-mode -1) ; disable menubar
(blink-cursor-mode 0) ; disable blinking cursor

(setq
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.5)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq
    gc-cons-threshold (* 1024 1024 8)
    gc-cons-percentage 0.1)))

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

;; ;; disable warnings
;; (setq warning-minimum-level :emergency)
;; (setq byte-compile-warnings '(cl-functions))

(provide 'early-init)
;;; early-init.el ends here
