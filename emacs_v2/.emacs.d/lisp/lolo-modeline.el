;;; lolo-modeline.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(require-package 'all-the-icons)

(require-package 'hide-mode-line)
(hide-mode-line-mode)

(require-package 'doom-modeline)
(doom-modeline-mode)
(setq doom-modeline-buffer-file-name-style 'relative-from-project ;; Just show file name (no path)
      doom-modeline-enable-word-count t
      doom-modeline-buffer-encoding t
      doom-modeline-icon t       ;; Enable/disable all icons
      doom-modeline-modal-icon t ;; Icon for Evil mode
      doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-buffer-state-icon t
      doom-modeline-bar-width 6)

(setq lolo-doom-modeline-text-height 100)
(setq doom-modeline-height 30)

(setq default-frame-alist '((left . 170)
                            (width . 173)
                            (top . 64)
                            (height . 53)
                            (fullscreen . fullheight)
                            (internal-border-width . 8)))

;; Window configuration
(setq frame-inhibit-implied-resize t) ;; Supposed to hasten startup

;; Less clutter (this is what dfrosted12 uses so I trust that)
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; This makes the Aqua titlebar color the same as Emacs.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil) ;; Otherwise shows a corner icon on the edge
(setq-default indicate-empty-lines nil) ;; Otherwise there are weird fringes on blank lines

(set-face-attribute 'fringe nil :background nil)
(set-face-attribute 'header-line nil :background nil :inherit 'default)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


(require-package 'visual-fill-column)
(setq visual-fill-column-center-text t)
(setq visual-fill-column-width 100)
(setq visual-fill-column-center-text t)

(require-package 'writeroom-mode)
(setq writeroom-maximize-window nil
      writeroom-mode-line t
      writeroom-global-effects nil ;; No need to have Writeroom do any of that silly stuff
      writeroom-extra-line-spacing 3) 
(setq writeroom-width visual-fill-column-width)

(provide 'lolo-modeline)
;;; lolo-modeline.el ends here
