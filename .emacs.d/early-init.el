;;; early-init.el --- xxx.	-*- lexical-binding: t -*-

;; Author: hoangzrik
;; URL: https://github.com/void-kun/dotfiles

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil)
(setq use-package-enable-imenu-support t)
(setq load-prefer-newer noninteractive)
;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)
;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;;(setq-default mode-line-format nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
