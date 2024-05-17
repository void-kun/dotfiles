;;; lolo-ui.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'lolo-const)
  (require 'lolo-custom))

;; Optimization
(setq idle-update-delay 1.0)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)

;; config font settings
(custom-set-faces
 '(default ((t (:family "ZedMono Nerd Font" :foundry "UKWN" :slant normal :weight regular :height 122 :width normal)))))


(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(add-to-list 'default-frame-alist '(font . "ZedMono Nerd Font-12"))
;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq
 frame-inhibit-implied-resize t
 frame-resize-pixelwise t)

;; Initial frame
(setq initial-frame-alist
      '((top . 0.5) (left . 0.5) (width . 0.628) (height . 0.8) (fullscreen)))

;; Title
(setq
 frame-title-format '("Lolo Emacs - %b")
 icon-title-format frame-title-format)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Mode-line
(use-package hide-mode-line)
(hide-mode-line-mode)

(use-package
  doom-modeline
  :config (doom-modeline-mode)
  (setq
   doom-modeline-buffer-file-name-style
   'relative-from-project ;; Just show file name (no path)
   doom-modeline-enable-word-count t
   doom-modeline-buffer-encoding t
   doom-modeline-icon t ;; Enable/disable all icons
   doom-modeline-modal-icon t ;; Icon for Evil mode
   doom-modeline-major-mode-icon nil
   doom-modeline-major-mode-color-icon t
   doom-modeline-buffer-state-icon t
   doom-modeline-bar-width 10))
(doom-modeline-mode)

(use-package nyan-mode)
(nyan-mode)

(setq lolo-doom-modeline-text-height 100)
(setq doom-modeline-height 35)

(setq default-frame-alist
      '((left . 170)
        (width . 173)
        (top . 64)
        (height . 53)
        (fullscreen . fullheight)
        (internal-border-width . 10)))

;; Window configuration
(setq frame-inhibit-implied-resize t) ;; Supposed to hasten startup

;; Less clutter (this is what dfrosted12 uses so I trust that)
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; This makes the Aqua titlebar color the same as Emacs.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(use-package all-the-icons)

(use-package
  kaolin-themes
  :config (setq kaolin-themes-modeline-border nil)
  :custom-face
  ;; Keep the modeline proper every time I use these themes.
  (mode-line ((t (:height ,lolo-doom-modeline-text-height))))
  (mode-line-inactive ((t (:height ,lolo-doom-modeline-text-height))))
  ;; Disable underline for org deadline warnings. I don't like the way it looks.
  (org-warning ((t (:underline nil))))
  ;; Darkens the org-ellipsis (first unset the color, then give it shadow)
  (org-ellipsis ((t (:foreground unspecified :height 0.8 :inherit 'shadow)))))

(use-package
  modus-themes
  :init
  (setq
   modus-themes-italic-constructs t
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
                (t . (rainbow)))))
  ;; (modus-themes-load-themes)
  :custom-face (org-ellipsis ((t (:height 0.8 :inherit 'shadow))))
  ;; Keep the modeline proper every time I use these themes.
  (mode-line ((t (:height ,lolo-doom-modeline-text-height))))
  (mode-line-inactive ((t (:height ,lolo-doom-modeline-text-height)))))

(use-package
  ef-themes
  :init
  (setq ef-themes-headings
        (quote ((1 . (variable-pitch 1.1))
                (2 . (regular))
                (3 . (regular))
                (4 . (regular)))))
  :custom-face (org-scheduled-today ((t (:inherit org-level-3)))))

;; A minor-mode menu for mode-line
(use-package minions :hook (doom-modeline-mode . minions-mode))

(use-package monokai-pro-theme)

(load-theme 'monokai-pro-ristretto t)

(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil) ;; Otherwise shows a corner icon on the edge
(setq-default indicate-empty-lines nil) ;; Otherwise there are weird fringes on blank lines

(set-face-attribute 'fringe nil :background nil)
(set-face-attribute 'header-line nil :background nil :inherit 'default)

;; Icons
(use-package
  nerd-icons
  :config
  (when (and (display-graphic-p) (not (font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))

;; Show line numbers
(use-package
  display-line-numbers
  :ensure nil
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init
  (setq display-line-numbers-width-start t)
  (setq display-line-numbers-type 'relative))

;; Suppress GUI features
(setq
 use-file-dialog nil
 use-dialog-box nil
 inhibit-startup-screen t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-scratch-message nil)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Display dividers between windows
(setq
 window-divider-default-places t
 window-divider-default-bottom-width 1
 window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Easily adjust the font size in all frames
(use-package
  default-text-scale
  :hook (after-init . default-text-scale-mode)
  :bind
  (:map
   default-text-scale-mode-map
   ("s-=" . default-text-scale-increase)
   ("s--" . default-text-scale-decrease)
   ("s-0" . default-text-scale-reset)
   ("C-s-=" . default-text-scale-increase)
   ("C-s--" . default-text-scale-decrease)
   ("C-s-0" . default-text-scale-reset)))

;; Display time
(use-package
  time
  :init
  (setq
   display-time-default-load-average nil
   display-time-format "%H:%M"))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq
   mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
   mouse-wheel-scroll-amount-horizontal 1
   mouse-wheel-progressive-speed nil))
(setq
 scroll-step 1
 scroll-margin 0
 scroll-conservatively 100000
 auto-window-vscroll nil
 scroll-preserve-screen-position t)

;; Good pixel line scrolling
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  (use-package
    good-scroll
    :diminish
    :hook (after-init . good-scroll-mode)
    :bind
    (([remap next] . good-scroll-up-full-screen)
     ([remap prior] . good-scroll-down-full-screen))))

(use-package iscroll :diminish :hook (image-mode . iscroll-mode))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch :diminish)

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package
  page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

;; Child frame
(when (childframe-workable-p)
  (use-package
    posframe
    :hook (after-load-theme . posframe-delete-all)
    :init
    (defface posframe-border `((t (:inherit region)))
      "Face used by the `posframe' border."
      :group 'posframe)
    (defvar posframe-border-width 2
      "Default posframe border width.")
    :config
    (with-no-warnings
      (defun my-posframe--prettify-frame (&rest _)
        (set-face-background 'fringe nil posframe--frame))
      (advice-add
       #'posframe--create-posframe
       :after #'my-posframe--prettify-frame)

      (defun posframe-poshandler-frame-center-near-bottom (info)
        (cons
         (/ (- (plist-get info :parent-frame-width)
               (plist-get info :posframe-width))
            2)
         (/ (+ (plist-get info :parent-frame-height)
               (* 2 (plist-get info :font-height)))
            2))))))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Ligatures support
(use-package
  composite
  :ensure nil
  :init (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode
     conf-mode
     nxml-mode
     markdown-mode
     help-mode
     shell-mode
     eshell-mode
     term-mode
     vterm-mode)
    .
    (lambda ()
      (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (let
      ((alist
        '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
          (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
          (36 . ".\\(?:\\(>\\)>?\\)")
          (37 . ".\\(?:\\(%\\)%?\\)")
          (38 . ".\\(?:\\(&\\)&?\\)")
          (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
          ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
          (43 . ".\\(?:\\([>]\\)>?\\)")
          ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
          (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
          ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
          (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
          (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
          ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
          (48 . ".\\(?:x[a-zA-Z]\\)")
          (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
          (59 . ".\\(?:\\(;\\);?\\)")
          (60
           .
           ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
          (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
          (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
          (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
          (91 . ".\\(?:\\(|\\)[]|]?\\)")
          ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
          (94 . ".\\(?:\\(=\\)=?\\)")
          (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
          (119 . ".\\(?:\\(ww\\)w?\\)")
          (123 . ".\\(?:\\(|\\)[|}]?\\)")
          (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
          (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range
       composition-ligature-table
       (car char-regexp)
       `([,(cdr char-regexp) 0 font-shape-gstring]))))
  (set-char-table-parent composition-ligature-table composition-function-table))


(provide 'lolo-ui)
;;; lolo-ui.el ends here
