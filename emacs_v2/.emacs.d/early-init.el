;;; early-init.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;; Compile warnings
(setq warning-minimum-level :emergency)
(setq native-comp-async-report-warnings-errors 'silent) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; (setq idle-update-delay 1.0)

(setq package-enable-at-startup nil)
(setq use-package-enable-imenu-support t)
(setq load-prefer-newer noninteractive)
(setq frame-inhibit-implied-resize t)

;; Disabling bidi (bidirectional editing stuff)
(setq-default bidi-display-reordering 'left-to-right 
              bidi-paragraph-direction 'left-to-right)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq inhibit-compacting-font-caches t)

(push '(menu-bar-lines . 0) default-frame-alist)
   (push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)
(setq auto-mode-case-fold nil)

;;; early-init.el ends here
