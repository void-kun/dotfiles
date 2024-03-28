;;; lolo-ui.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(line-number-mode)
(column-number-mode)
(display-time-mode -1)
(size-indication-mode 0)

(setq text-scale-mode-step 1.2) ;; How much to adjust text scale by when using `text-scale-mode'
(setq lolo-default-line-spacing 1) ;; This happens in the variables but I guess I have it here too.

(setq-default line-spacing lolo-default-line-spacing)

;; Setting text size based on the computer I am on.
(setq lolo-text-height 150)
(set-frame-font "RobotoMono Nerd Font:size=14" nil t)

(use-package mixed-pitch
  :defer t
  :config
  (setq mixed-pitch-set-height nil)
  (dolist (face '(org-date org-priority org-tag org-special-keyword)) ;; Some extra faces I like to be fixed-pitch
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;; Disables showing system load in modeline, useless anyway
(setq display-time-default-load-average nil)

(global-visual-line-mode t)

(line-number-mode)
(column-number-mode)
(display-time-mode -1)
(size-indication-mode 0)

(use-package hide-mode-line
  :commands (hide-mode-line-mode))

(use-package doom-modeline
  :config
  (doom-modeline-mode)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project ;; Just show file name (no path)
        doom-modeline-enable-word-count t
        doom-modeline-buffer-encoding t
        doom-modeline-icon t ;; Enable/disable all icons
        doom-modeline-modal-icon t ;; Icon for Evil mode
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-bar-width 6))

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

(use-package all-the-icons)

(use-package doom-themes
  :after mixed-pitch
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  :custom-face
  (org-ellipsis ((t (:height 0.8 :inherit 'shadow))))
  ;; Keep the modeline proper every time I use these themes.
  (mode-line ((t (:height ,lolo-doom-modeline-text-height))))
  (mode-line-inactive ((t (:height ,lolo-doom-modeline-text-height))))
  (doom-modeline ((t (:height ,lolo-doom-modeline-text-height))))
  (doom-modeline-inactive ((t (:height ,lolo-doom-modeline-text-height))))
  (org-scheduled-previously ((t (:background "red")))))

(use-package kaolin-themes
  :config
  (setq kaolin-themes-modeline-border nil)
  :custom-face
  ;; Keep the modeline proper every time I use these themes.
  (mode-line ((t (:height ,lolo-doom-modeline-text-height))))
  (mode-line-inactive ((t (:height ,lolo-doom-modeline-text-height))))
  ;; Disable underline for org deadline warnings. I don't like the way it looks.
  (org-warning ((t (:underline nil))))
  ;; Darkens the org-ellipsis (first unset the color, then give it shadow)
  (org-ellipsis ((t (:foreground unspecified :height 0.8 :inherit 'shadow)))))

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend)
        modus-themes-hl-line '(intense) ;; accented or intense
        modus-themes-syntax '(yellow-comments)
        modus-themes-org-blocks 'gray-background
        modus-themes-mode-line '(moody borderless)) ;; moody or accented is what I use

  (setq modus-themes-headings ;; Makes org headings more colorful
        '((t . (rainbow))))

  (setq modus-themes-headings
        (quote ((1 . (variable-pitch 1.1 rainbow))
                (2 . (regular))
                (3 . (regular))
                (4 . (regular))
                (t . (rainbow))
                )))
  ;; (modus-themes-load-themes)
  :custom-face
  (org-ellipsis ((t (:height 0.8 :inherit 'shadow))))
  ;; Keep the modeline proper every time I use these themes.
  (mode-line ((t (:height ,lolo-doom-modeline-text-height))))
  (mode-line-inactive ((t (:height ,lolo-doom-modeline-text-height)))))

(use-package ef-themes
  :init
  (setq ef-themes-headings
        (quote ((1 . (variable-pitch 1.1))
                (2 . (regular))
                (3 . (regular))
                (4 . (regular)))))
  :custom-face
  (org-scheduled-today ((t (:inherit org-level-3)))))


;; loading theme based on the time.
(let ((hour (string-to-number (substring (current-time-string) 11 13))))
  (if (or (> hour 17) (< hour 6))
      (load-theme 'kaolin-valley-dark t) ;; Night
    (load-theme 'kaolin-valley-light t))) ;; Day

(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil) ;; Otherwise shows a corner icon on the edge
(setq-default indicate-empty-lines nil) ;; Otherwise there are weird fringes on blank lines

(set-face-attribute 'fringe nil :background nil)
(set-face-attribute 'header-line nil :background nil :inherit 'default)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package visual-fill-column
  :defer t
  :config
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text t))

(use-package writeroom-mode
  :defer t
  :config
  (setq writeroom-maximize-window nil
        writeroom-mode-line t
        writeroom-global-effects nil ;; No need to have Writeroom do any of that silly stuff
        writeroom-extra-line-spacing 3) 
  (setq writeroom-width visual-fill-column-width))

(defun my-presentation-on ()
  (setq lolo-default-line-spacing 3)
  (setq-default line-spacing lolo-default-line-spacing)
  (setq-local line-spacing lolo-default-line-spacing)
  (setq ivy-height 6))

(defun my-presentation-off ()
  (lolo/reset-var 'lolo-default-line-spacing)
  (setq-default line-spacing lolo-default-line-spacing)
  (setq-local line-spacing lolo-default-line-spacing)
  (lolo/reset-var 'ivy-height))

(add-hook 'presentation-on-hook #'my-presentation-on)
(add-hook 'presentation-off-hook #'my-presentation-off)

(setq presentation-default-text-scale 5)
(use-package presentation :defer t)

(provide 'lolo-ui)
;;; lolo-ui.el ends here
