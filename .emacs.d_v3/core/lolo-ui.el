;; ;; lolo-ui.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'lolo-vars))

;; font face
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(add-to-list 'default-frame-alist '(font . "XD Semibold-14"))
(setq-default
 line-height 150
 line-spacing 4)

;; nice scrolling
(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; whitespace
(when lolo-whitespace
  (global-whitespace-mode 1)
  (setq-default whitespace-style
                '(face
                  spaces
                  empty
                  tabs
                  trailing
                  space-mark
                  tab-mark
                  newline-mark))

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
                      occur-mode)))

;; Show line numbers
(use-package
 display-line-numbers
 :ensure nil
 :hook
 ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
 :init (setq display-line-numbers-width-start t))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Suppress GUI features
(setq
 use-file-dialog nil
 use-dialog-box nil
 inhibit-startup-screen t
 inhibit-default-init t
 initial-major-mode 'text-mode
 initial-scratch-message "Present Day, Present Time...\n")

;; Display dividers between windows
(setq
 window-divider-default-places t
 window-divider-default-bottom-width 1
 window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(setq frame-title-format
      '("" invocation-name " Lolo - "
        (:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

;; Icons
(use-package
 nerd-icons
 :config
 (when (and (display-graphic-p)
            (not (font-installed-p nerd-icons-font-family)))
   (nerd-icons-install-fonts t)))

(use-package all-the-icons)

;; themes
(when lolo-theme
  (require
   (pcase lolo-theme
     ('gruber-darker 'lolo-gruber-darker-theme)
     ('ef 'lolo-ef-theme)
     ('modus 'lolo-modus-theme)
     ('standard 'lolo-standard-theme))))

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

;; Mode-line
(use-package
 doom-modeline
 :hook (after-init . doom-modeline-mode)
 :init
 (setq
  doom-modeline-icon t
  doom-modeline-minor-modes t)
 :config
 (setq doom-modeline-support-imenu t)
 (setq doom-modeline-height 36)
 (setq doom-modeline-bar-width 4)
 (setq doom-modeline-hud nil)
 (setq doom-modeline-window-width-limit 85)
 (setq doom-modeline-project-detection 'auto)
 (setq doom-modeline-buffer-file-name-style 'auto)
 (setq inhibit-compacting-font-caches t)
 (setq find-file-visit-truename t)

 (custom-set-faces
  '(mode-line ((t (:family "XD" :height 1.0))))
  '(mode-line-active ((t (:family "XD" :height 1.0)))) ; For 29+
  '(mode-line-inactive
    ((t (:family "RobotoMono Nerd Font" :height 1.0))))))

(use-package
 hide-mode-line
 :hook
 (((treemacs-mode
    eshell-mode
    shell-mode
    term-mode
    vterm-mode
    embark-collect-mode
    pdf-annot-list-mode)
   . turn-on-hide-mode-line-mode)
  (dired-mode
   .
   (lambda ()
     (and (bound-and-true-p hide-mode-line-mode)
          (turn-off-hide-mode-line-mode))))))

;; A minor-mode menu for mode-line
(use-package minions :hook (doom-modeline-mode . minions-mode))

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

;; Enforce rules for popups
(use-package
 popper
 :custom
 (popper-group-function #'popper-group-by-directory)
 (popper-echo-dispatch-actions t)
 :bind
 (:map
  popper-mode-map
  ("C-h z" . popper-toggle)
  ("C-<tab>" . popper-cycle)
  ("C-M-<tab>" . popper-toggle-type))
 :hook (emacs-startup . popper-echo-mode)
 :init
 (setq popper-reference-buffers
       '("\\*Messages\\*$"
         "Output\\*$"
         "\\*Pp Eval Output\\*$"
         "^\\*eldoc.*\\*$"
         "\\*Compile-Log\\*$"
         "\\*Completions\\*$"
         "\\*Warnings\\*$"
         "\\*Async Shell Command\\*$"
         "\\*Apropos\\*$"
         "\\*Backtrace\\*$"
         "\\*Calendar\\*$"
         "\\*Fd\\*$"
         "\\*Find\\*$"
         "\\*Finder\\*$"
         "\\*Kill Ring\\*$"
         "\\*Embark \\(Collect\\|Live\\):.*\\*$"

         bookmark-bmenu-mode
         comint-mode
         compilation-mode
         help-mode
         helpful-mode
         tabulated-list-mode
         Buffer-menu-mode

         flycheck-error-list-mode
         flycheck-verify-mode

         gnus-article-mode
         devdocs-mode
         grep-mode
         occur-mode
         rg-mode
         deadgrep-mode
         ag-mode
         pt-mode
         youdao-dictionary-mode
         osx-dictionary-mode
         fanyi-mode
         "^\\*gt-result\\*$"
         "^\\*gt-log\\*$"

         "^\\*Process List\\*$"
         process-menu-mode
         list-environment-mode
         cargo-process-mode

         "^\\*.*eshell.*\\*.*$"
         "^\\*.*shell.*\\*.*$"
         "^\\*.*terminal.*\\*.*$"
         "^\\*.*vterm[inal]*.*\\*.*$"

         "\\*DAP Templates\\*$"
         dap-server-log-mode
         "\\*ELP Profiling Restuls\\*"
         profiler-report-mode
         "\\*Paradox Report\\*$"
         "\\*package update results\\*$"
         "\\*Package-Lint\\*$"
         "\\*[Wo]*Man.*\\*$"
         "\\*ert\\*$"
         overseer-buffer-mode
         "\\*gud-debug\\*$"
         "\\*quickrun\\*$"
         "\\*tldr\\*$"
         "\\*vc-.*\\**"
         "\\*diff-hl\\**"
         "^\\*macro expansion\\**"

         "\\*Agenda Commands\\*"
         "\\*Org Select\\*"
         "\\*Capture\\*"
         "^CAPTURE-.*\\.org*"
         "\\*Gofmt Errors\\*$"
         "\\*Go Test\\*$"
         godoc-mode
         "\\*docker-.+\\*"
         "\\*prolog\\*"
         inferior-python-mode
         inf-ruby-mode
         swift-repl-mode
         "\\*rustfmt\\*$"
         rustic-compilation-mode
         rustic-cargo-clippy-mode
         rustic-cargo-outdated-mode
         rustic-cargo-run-mode
         rustic-cargo-test-mode))

 (with-eval-after-load 'doom-modeline
   (setq popper-mode-line
         '(:eval
           (let ((face
                  (if (doom-modeline--active)
                      'doom-modeline-emphasis
                    'doom-modeline)))
             (if (and (bound-and-true-p doom-modeline-icon)
                      (bound-and-true-p doom-modeline-mode))
                 (format " %s "
                         (nerd-icons-octicon "nf-oct-pin" :face face))
               (propertize " POP " 'face face))))))
 :config
 (with-no-warnings
   (defun my-popper-fit-window-height (win)
     "Determine the height of popup window WIN by fitting it to the buffer's content."
     (fit-window-to-buffer
      win (floor (frame-height) 3) (floor (frame-height) 3)))
   (setq popper-window-height #'my-popper-fit-window-height)

   (defun popper-close-window-hack (&rest _)
     "Close popper window via `C-g'."
     (when (and (called-interactively-p 'interactive)
            (not (region-active-p))
            popper-open-popup-alist)
       (when-let ((window (caar popper-open-popup-alist))
                  (buffer (cdar popper-open-popup-alist)))
         (when (and (with-current-buffer buffer
                      (not
                       (derived-mode-p
                        'eshell-mode
                        'shell-mode
                        'term-mode
                        'vterm-mode)))
                    (window-live-p window))
           (delete-window window)))))
   (advice-add #'keyboard-quit :before #'popper-close-window-hack)))

(provide 'lolo-ui)
;;; lolo-ui.el ends here
