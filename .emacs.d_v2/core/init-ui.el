;;; init-ui.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(custom-set-faces
 '(default
   ((t
     (:family
      "XD"
      :foundry "CYEL"
      :slant normal
      :weight semi-bold
      :height 100
      :width normal)))))

;; Uncomment the following line if line spacing needs adjusting.
;; (setq-default line-spacing 0.12)

;; disable startup screen
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "Present Day, Present Time...\n")

;; nice scrolling
(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; there's a built-in linum-mode, but we're using
;; display-line-numbers-mode or nlinum-mode,
;; as it's supposedly faster
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode)
  (global-nlinum-mode t))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Lolo - "
        (:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

;; icons
(use-package all-the-icons :if (display-graphic-p))

;; Icons
(use-package nerd-icons
  :if (display-graphic-p))

;; themes
(when lolo-theme
  (require
   (pcase lolo-theme
     ('ef 'prot-emacs-ef-themes)
     ('modus 'prot-emacs-modus-themes)
     ('standard 'prot-emacs-standard-themes))))

;; dashboard
(use-package
 dashboard
 :demand
 :diminish (dashboard-mode page-break-lines-mode)
 :bind
 (("C-z d" . open-dashboard)
  :map dashboard-mode-map
  (("n" . dashboard-next-line)
   ("p" . dashboard-previous-line)
   ("N" . dashboard-next-section)
   ("F" . dashboard-previous-section)))
 :custom (dashboard-banner-logo-title "")
 (dashboard-startup-banner
  (expand-file-name "banner.txt" user-emacs-directory))
 (dashboard-items '((recents . 7) (bookmarks . 7) (agenda . 5)))
 (initial-buffer-choice
  (lambda () (get-buffer dashboard-buffer-name)))
 (dashboard-set-heading-icons t) (dashboard-set-navigator t)
 :custom-face (dashboard-banner-logo-title ((t (:family "XD" :height 123))))
 :config (dashboard-setup-startup-hook)
 ;; Open Dashboard function
 (defun open-dashboard ()
   "Open the *dashboard* buffer and jump to the first widget."
   (interactive)
   (dashboard-insert-startupify-lists)
   (switch-to-buffer dashboard-buffer-name)
   (goto-char (point-min))
   (delete-other-windows)))

;; page break
(use-package
 page-break-lines
 :diminish
 :init (global-page-break-lines-mode))

;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; highlight line
(global-hl-line-mode 1)

;; pretty symbols
(global-prettify-symbols-mode 1)
(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols.  See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(("lambda" . 955)
          ("delta" . 120517)
          ("epsilon" . 120518)
          ("->" . 8594)
          ("<=" . 8804)
          (">=" . 8805))))
(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; ============================================================================
;; Modeline.

(use-package
 prot-modeline
 :load-path
 (lambda ()
   (expand-file-name "site-elisp/prot-modeline" user-emacs-directory))
 :ensure nil
 :config
 (setq mode-line-compact nil) ; Emacs 28
 (setq mode-line-right-align-edge 'right-margin) ; Emacs 30
 (setq-default mode-line-format
               '("%e"
                 prot-modeline-kbd-macro
                 prot-modeline-narrow
                 prot-modeline-buffer-status
                 prot-modeline-window-dedicated-status
                 prot-modeline-input-method
                 "  "
                 prot-modeline-buffer-identification
                 "  "
                 prot-modeline-major-mode
                 prot-modeline-process
                 "  "
                 prot-modeline-vc-branch
                 "  "
                 prot-modeline-eglot
                 "  "
                 prot-modeline-flymake
                 "  "
                 mode-line-format-right-align ; Emacs 30
                 prot-modeline-notmuch-indicator
                 "  "
                 prot-modeline-misc-info))

 (with-eval-after-load 'spacious-padding
   (defun prot/modeline-spacious-indicators ()
     "Set box attribute to `'prot-modeline-indicator-button' if spacious-padding is enabled."
     (if (bound-and-true-p spacious-padding-mode)
         (set-face-attribute 'prot-modeline-indicator-button nil
                             :box t)
       (set-face-attribute 'prot-modeline-indicator-button nil
                           :box 'unspecified)))

   ;; Run it at startup and then afterwards whenever
   ;; `spacious-padding-mode' is toggled on/off.
   (prot/modeline-spacious-indicators)

   (add-hook
    'spacious-padding-mode-hook #'prot/modeline-spacious-indicators)))

;;; Keycast mode
(use-package
 keycast
 :ensure t
 :after prot-modeline
 :commands
 (keycast-mode-line-mode
  keycast-header-line-mode keycast-tab-bar-mode keycast-log-mode)
 :init
 (setq keycast-mode-line-format "%2s%k%c%R")
 (setq keycast-mode-line-insert-after 'prot-modeline-vc-branch)
 (setq keycast-mode-line-window-predicate
       'mode-line-window-selected-p)
 (setq keycast-mode-line-remove-tail-elements nil)
 :config
 (dolist (input '(self-insert-command org-self-insert-command))
   (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

 (dolist (event
          '(mouse-event-p mouse-movement-p
                          mwheel-scroll
                          handle-select-window
                          mouse-set-point
                          mouse-drag-region))
   (add-to-list 'keycast-substitute-alist `(,event nil))))

;;;; Pulsar
;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
(use-package pulsar
  :ensure t
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)

  (pulsar-global-mode 1)
  :hook
  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-red))
  :bind
  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (("C-x l" . pulsar-pulse-line) ; override `count-lines-page'
   ("C-x L" . pulsar-highlight-dwim))) ; or use `pulsar-highlight-line'

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(use-package lin
  :ensure t
  :hook (after-init . lin-global-mode) ; applies to all `lin-mode-hooks'
  :config
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  ;;
  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
  ;;
  ;; I still prefer `setq' for consistency.
  (setq lin-face 'lin-magenta))

;;;; Increase padding of windows/frames
;; Yet another one of my packages:
;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . spacious-padding-mode)
  :bind ("<f8>" . spacious-padding-mode)
  :init
  ;; These are the defaults, but I keep it here for visiibility.
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 1
           :scroll-bar-width 8
           :left-fringe-width 20
           :right-fringe-width 20))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as
  ;; it is very flexible.
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active ,(if (or (eq lolo-theme 'modus)
                                      (eq lolo-theme 'standard))
                                  'default
                                'help-key-binding)
           :mode-line-inactive window-divider)))

;;;; Rainbow mode for colour previewing (rainbow-mode.el)
(use-package rainbow-mode
  :ensure t
  :init
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)

  (defun prot/rainbow-mode-in-themes ()
    (when-let ((file (buffer-file-name))
               ((derived-mode-p 'emacs-lisp-mode))
               ((string-match-p "-theme" file)))
      (rainbow-mode 1)))
  :bind ( :map ctl-x-x-map
          ("c" . rainbow-mode)) ; C-x x c
  :hook (emacs-lisp-mode . prot/rainbow-mode-in-themes))

;;; Cursor appearance (cursory)
;; Read the manual: <https://protesilaos.com/emacs/cursory>.
(use-package cursory
  :ensure t
  :demand t
  :if (display-graphic-p)
  :config
  (setq cursory-presets
        '((box
           :blink-cursor-interval 1.2)
          (box-no-blink
           :blink-cursor-mode -1)
          (bar
           :cursor-type (bar . 2)
           :blink-cursor-interval 0.8)
          (bar-no-other-window
           :inherit bar
           :cursor-in-non-selected-windows nil)
          (bar-no-blink
           :cursor-type (bar . 2)
           :blink-cursor-mode -1)
          (underscore
           :cursor-type (hbar . 3)
           :blink-cursor-blinks 50)
          (underscore-thin-other-window
           :inherit underscore
           :cursor-in-non-selected-windows (hbar . 1))
          (underscore-thick
           :cursor-type (hbar . 8)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50
           :cursor-in-non-selected-windows (hbar . 3))
          (underscore-thick-no-blink
           :blink-cursor-mode -1
           :cursor-type (hbar . 8)
           :cursor-in-non-selected-windows (hbar . 3))
          (t ; the default values
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  ;; I am using the default values of `cursory-latest-state-file'.

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))
  :hook
  ;; The other side of `cursory-restore-latest-preset'.
  (kill-emacs . cursory-store-latest-preset)
  :bind
  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture' and is the one I use as well.
  ("C-c p" . cursory-set-preset))

;;;; Theme buffet
(use-package theme-buffet
  :ensure t
  :after (:any modus-themes ef-themes)
  :defer 1
  :config
  (let ((modus-themes-p (featurep 'modus-themes))
        (ef-themes-p (featurep 'ef-themes)))
    (setq theme-buffet-menu 'end-user)
    (setq theme-buffet-end-user
          (cond
           ((and modus-themes-p ef-themes-p)
            '( :night     (modus-vivendi ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis)
               :morning   (modus-operandi ef-light ef-cyprus ef-spring ef-frost ef-duo-light)
               :afternoon (modus-operandi-tinted ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
               :evening   (modus-vivendi-tinted ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))
           (ef-themes-p
            '( :night     (ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis)
               :morning   (ef-light ef-cyprus ef-spring ef-frost ef-duo-light)
               :afternoon (ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
               :evening   (ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))
           (modus-themes-p
            '( :night     (modus-vivendi modus-vivendi-tinted modus-vivendi-tritanopia modus-vivendi-deuteranopia)
               :morning   (modus-operandi modus-operandi-tinted modus-operandi-tritanopia modus-operandi-deuteranopia)
               :afternoon (modus-operandi modus-operandi-tinted modus-operandi-tritanopia modus-operandi-deuteranopia)
               :evening   (modus-vivendi modus-vivendi-tinted modus-vivendi-tritanopia modus-vivendi-deuteranopia)))))

    (when (or modus-themes-p ef-themes-p)
      (theme-buffet-timer-hours 1))))

;;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :hook
  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  ((after-init . fontaine-mode)
   (after-init . (lambda ()
                        ;; Set last preset or fall back to desired style from `fontaine-presets'.
                        (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
  :bind ("C-c f" . fontaine-set-preset)
  :config
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  (setq fontaine-presets
        '((small
           :default-family "XD"
           :default-height 80
           :variable-pitch-family "XD")
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-weight semilight
           :default-height 115
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 150)
          (live-stream
           :default-family "XD"
           :default-height 150
           :default-weight medium
           :fixed-pitch-family "XD"
           :variable-pitch-family "XD"
           :bold-weight extrabold)
          (presentation
           :default-height 180)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "XD"
           :default-weight regular
           :default-slant normal
           :default-height 100

           :fixed-pitch-family "XD"
           :fixed-pitch-weight nil
           :fixed-pitch-slant nil
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-slant nil
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "XD"
           :variable-pitch-weight nil
           :variable-pitch-slant nil
           :variable-pitch-height 1.0

           :mode-line-active-family nil
           :mode-line-active-weight nil
           :mode-line-active-slant nil
           :mode-line-active-height 1.0

           :mode-line-inactive-family nil
           :mode-line-inactive-weight nil
           :mode-line-inactive-slant nil
           :mode-line-inactive-height 1.0

           :header-line-family nil
           :header-line-weight nil
           :header-line-slant nil
           :header-line-height 1.0

           :line-number-family nil
           :line-number-weight nil
           :line-number-slant nil
           :line-number-height 1.0

           :tab-bar-family nil
           :tab-bar-weight nil
           :tab-bar-slant nil
           :tab-bar-height 1.0

           :tab-line-family nil
           :tab-line-weight nil
           :tab-line-slant nil
           :tab-line-height 1.0

           :bold-family nil
           :bold-weight bold
           :bold-slant nil
           :bold-height 1.0

           :italic-family nil
           :italic-weight nil
           :italic-slant italic
           :italic-height 1.0

           :line-spacing nil)))

  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

;;;;; `variable-pitch-mode' setup
(use-package face-remap
  :ensure nil
  :functions prot/enable-variable-pitch
  :bind ( :map ctl-x-x-map
          ("v" . variable-pitch-mode))
  :hook ((text-mode notmuch-show-mode elfeed-show-mode) . prot/enable-variable-pitch)
  :config
  ;; NOTE 2022-11-20: This may not cover every case, though it works
  ;; fine in my workflow.  I am still undecided by EWW.
  (defun prot/enable-variable-pitch ()
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
      (variable-pitch-mode 1)))
;;;;; Resize keys with global effect
  :bind
  ;; Emacs 29 introduces commands that resize the font across all
  ;; buffers (including the minibuffer), which is what I want, as
  ;; opposed to doing it only in the current buffer.  The keys are the
  ;; same as the defaults.
  (("C-x C-=" . global-text-scale-adjust)
   ("C-x C-+" . global-text-scale-adjust)
   ("C-x C-0" . global-text-scale-adjust)))

(provide 'init-ui)
;;; init-ui.el ends here
