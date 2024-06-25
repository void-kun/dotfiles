;;; init-ui.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

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

(add-to-list 'default-frame-alist '(font . "XD SemiBold-10"))
(setq-default line-height 150
              line-spacing 4)

;; whitespace
;; Define the whitespace style.
(setq-default whitespace-style
              '(face spaces empty tabs trailing space-mark tab-mark newline-mark))

(setq-default whitespace-display-mappings
      '(
        ;; space -> · else .
        (space-mark 32 [183] [46])
        ;; tabs -> » else >
        (tab-mark ?\t [187 ?\t] [62 ?\t])))

;; Don't enable whitespace for.
(setq-default whitespace-global-modes
              '(not shell-mode
                    help-mode
                    magit-mode
                    magit-diff-mode
                    ibuffer-mode
                    dired-mode
                    occur-mode))

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

;;;;; `variable-pitch-mode' setup
(use-package face-remap
  :ensure nil
  :functions prot/enable-variable-pitch
  :bind ( :map ctl-x-x-map
          ("v" . variable-pitch-mode))
  :hook ((text-mode notmuch-show-mode elfeed-show-mode) . prot/enable-variable-pitch)
  :config
  (defun prot/enable-variable-pitch ()
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
      (variable-pitch-mode 1)))
  :bind
  (("C-x C-=" . global-text-scale-adjust)
   ("C-x C-+" . global-text-scale-adjust)
   ("C-x C-0" . global-text-scale-adjust)))

(provide 'init-ui)
;;; init-ui.el ends here
