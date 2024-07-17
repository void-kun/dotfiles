;;; lolo-ui.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
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

(add-to-list 'default-frame-alist '(font . "XD Medium-12"))
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
     ('nano 'lolo-nano-theme)
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

;; hydra
(use-package
 hydra
 :hook (emacs-lisp-mode . hydra-add-imenu)
 :init
 (when (childframe-completion-workable-p)
   (setq hydra-hint-display-type 'posframe)

   (with-eval-after-load 'posframe
     (defun hydra-set-posframe-show-params ()
       "Set hydra-posframe style."
       (setq
        hydra-posframe-show-params
        `(:left-fringe
          8
          :right-fringe 8
          :internal-border-width 2
          :internal-border-color ,(face-background 'posframe-border nil t)
          :background-color ,(face-background 'tooltip nil t)
          :foreground-color ,(face-foreground 'tooltip nil t)
          :lines-truncate t
          :poshandler posframe-poshandler-frame-center-near-bottom)))
     (hydra-set-posframe-show-params)
     (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params
               t))))

(use-package
 pretty-hydra
 :custom (pretty-hydra-default-title-body-format-spec " %s%s")
 :bind ("<f6>" . toggles-hydra/body)
 :hook
 (emacs-lisp-mode
  .
  (lambda ()
    (add-to-list
     'imenu-generic-expression
     '("Hydras" "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)" 2))))
 :init
 (cl-defun
  pretty-hydra-title
  (title &optional icon-type icon-name &key face height v-adjust)
  "Add an icon in the hydra title."
  (let ((face (or face `(:inherit highlight :reverse-video t)))
        (height (or height 1.2))
        (v-adjust (or v-adjust 0.0)))
    (concat
     (when (and icon-type icon-name)
       (let ((f (intern (format "nerd-icons-%s" icon-type))))
         (when (fboundp f)
           (concat
            (apply f
                   (list
                    icon-name
                    :face face
                    :height height
                    :v-adjust v-adjust))
            " "))))
     (propertize title 'face face))))

 ;; Global toggles
 (with-no-warnings
   (pretty-hydra-define
    toggles-hydra
    (:title
     (pretty-hydra-title "Toggles" 'faicon "nf-fa-toggle_on")
     :color amaranth
     :quit-key ("q" "C-g"))
    ("Basic" (("n" (cond
        ((fboundp 'display-line-numbers-mode)
         (display-line-numbers-mode
          (if display-line-numbers-mode
              -1
            1)))
        ((fboundp 'gblobal-linum-mode)
         (global-linum-mode
          (if global-linum-mode
              -1
            1))))
       "line number"
       :toggle
       (or (bound-and-true-p display-line-numbers-mode)
           (bound-and-true-p global-linum-mode)))
      ("a"
       global-aggressive-indent-mode
       "aggressive indent"
       :toggle t)
      ("d" global-hungry-delete-mode "hungry delete" :toggle t)
      ("e" electric-pair-mode "electric pair" :toggle t)
      ("c" flyspell-mode "spell check" :toggle t)
      ("s" prettify-symbols-mode "pretty symbol" :toggle t)
      ("l" global-page-break-lines-mode "page break lines" :toggle t)
      ("b" display-battery-mode "battery" :toggle t)
      ("i" display-time-mode "time" :toggle t)
      ("m" doom-modeline-mode "modern mode-line" :toggle t))
     "Highlight"
     (("h l" global-hl-line-mode "line" :toggle t)
      ("h p" show-paren-mode "paren" :toggle t)
      ("h s" symbol-overlay-mode "symbol" :toggle t)
      ("h r" rainbow-mode "rainbow" :toggle t)
      ("h w" (setq-default show-trailing-whitespace
                     (not show-trailing-whitespace))
       "whitespace"
       :toggle show-trailing-whitespace)
      ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
      ("h i" highlight-indent-guides-mode "indent" :toggle t)
      ("h t" global-hl-todo-mode "todo" :toggle t))
     "Program"
     (("f" flymake-mode "flymake" :toggle t)
      ("O" hs-minor-mode "hideshow" :toggle t)
      ("u" subword-mode "subword" :toggle t)
      ("W" which-function-mode "which function" :toggle t)
      ("E"
       toggle-debug-on-error
       "debug on error"
       :toggle (default-value 'debug-on-error))
      ("Q"
       toggle-debug-on-quit
       "debug on quit"
       :toggle (default-value 'debug-on-quit))
      ("v" global-diff-hl-mode "gutter" :toggle t)
      ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
      ("M" diff-hl-margin-mode "margin gutter" :toggle t)
      ("D" diff-hl-dired-mode "dired gutter" :toggle t))))))

;; Mode-line
(use-package
 doom-modeline
 :hook (after-init . doom-modeline-mode)
 :init
 (setq
  doom-modeline-icon t
  doom-modeline-minor-modes t)
 :bind (:map doom-modeline-mode-map ("C-<f6>" . doom-modeline-hydra/body))
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
  '(mode-line-inactive ((t (:family "RobotoMono Nerd Font" :height 1.0)))))
 :pretty-hydra
 ((:title
   (pretty-hydra-title
    "Mode Line"
    'sucicon
    "nf-custom-emacs"
    :face 'nerd-icons-purple)
   :color amaranth
   :quit-key ("q" "C-g"))
  ("Icon" (("i"
     (setq doom-modeline-icon (not doom-modeline-icon))
     "display icons"
     :toggle doom-modeline-icon)
    ("u" (setq doom-modeline-unicode-fallback
           (not doom-modeline-unicode-fallback))
     "unicode fallback"
     :toggle doom-modeline-unicode-fallback)
    ("m" (setq doom-modeline-major-mode-icon
           (not doom-modeline-major-mode-icon))
     "major mode"
     :toggle doom-modeline-major-mode-icon)
    ("c" (setq doom-modeline-major-mode-color-icon
           (not doom-modeline-major-mode-color-icon))
     "colorful major mode"
     :toggle doom-modeline-major-mode-color-icon)
    ("s" (setq doom-modeline-buffer-state-icon
           (not doom-modeline-buffer-state-icon))
     "buffer state"
     :toggle doom-modeline-buffer-state-icon)
    ("o" (setq doom-modeline-buffer-modification-icon
           (not doom-modeline-buffer-modification-icon))
     "modification"
     :toggle doom-modeline-buffer-modification-icon)
    ("x"
     (setq doom-modeline-time-icon (not doom-modeline-time-icon))
     "time"
     :toggle doom-modeline-time-icon)
    ("v"
     (setq doom-modeline-modal-icon (not doom-modeline-modal-icon))
     "modal"
     :toggle doom-modeline-modal-icon))
   "Segment"
   (("g h"
     (setq doom-modeline-hud (not doom-modeline-hud))
     "hud"
     :toggle doom-modeline-hud)
    ("g m"
     (setq doom-modeline-minor-modes (not doom-modeline-minor-modes))
     "minor modes"
     :toggle doom-modeline-minor-modes)
    ("g w" (setq doom-modeline-enable-word-count
           (not doom-modeline-enable-word-count))
     "word count"
     :toggle doom-modeline-enable-word-count)
    ("g e" (setq doom-modeline-buffer-encoding
           (not doom-modeline-buffer-encoding))
     "encoding"
     :toggle doom-modeline-buffer-encoding)
    ("g i"
     (setq doom-modeline-indent-info (not doom-modeline-indent-info))
     "indent"
     :toggle doom-modeline-indent-info)
    ("g c" (setq doom-modeline-display-misc-in-all-mode-lines
           (not doom-modeline-display-misc-in-all-mode-lines))
     "misc info"
     :toggle doom-modeline-display-misc-in-all-mode-lines)
    ("g l"
     (setq doom-modeline-lsp (not doom-modeline-lsp))
     "lsp"
     :toggle doom-modeline-lsp)
    ("g k" (setq doom-modeline-workspace-name
           (not doom-modeline-workspace-name))
     "workspace"
     :toggle doom-modeline-workspace-name)
    ("g g"
     (setq doom-modeline-github (not doom-modeline-github))
     "github"
     :toggle doom-modeline-github)
    ("g n"
     (setq doom-modeline-gnus (not doom-modeline-gnus))
     "gnus"
     :toggle doom-modeline-gnus)
    ("g r"
     (setq doom-modeline-irc (not doom-modeline-irc))
     "irc"
     :toggle doom-modeline-irc)
    ("g f"
     (setq doom-modeline-irc-buffers (not doom-modeline-irc-buffers))
     "irc buffers"
     :toggle doom-modeline-irc-buffers)
    ("g s" (progn
       (setq doom-modeline-check-simple-format
             (not doom-modeline-check-simple-format))
       (and (bound-and-true-p flycheck-mode) (flycheck-buffer)))
     "simple check format"
     :toggle doom-modeline-check-simple-format)
    ("g t"
     (setq doom-modeline-time (not doom-modeline-time))
     "time"
     :toggle doom-modeline-time)
    ("g v"
     (setq doom-modeline-env-version (not doom-modeline-env-version))
     "version"
     :toggle doom-modeline-env-version))
   "Style"
   (("a"
     (setq doom-modeline-buffer-file-name-style 'auto)
     "auto"
     :toggle (eq doom-modeline-buffer-file-name-style 'auto))
    ("b"
     (setq doom-modeline-buffer-file-name-style 'buffer-name)
     "buffer name"
     :toggle (eq doom-modeline-buffer-file-name-style 'buffer-name))
    ("f"
     (setq doom-modeline-buffer-file-name-style 'file-name)
     "file name"
     :toggle (eq doom-modeline-buffer-file-name-style 'file-name))
    ("F" (setq doom-modeline-buffer-file-name-style
           'file-name-with-project)
     "file name with project"
     :toggle
     (eq
      doom-modeline-buffer-file-name-style 'file-name-with-project))
    ("t u" (setq doom-modeline-buffer-file-name-style
           'truncate-upto-project)
     "truncate upto project"
     :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-project))
    ("t f" (setq doom-modeline-buffer-file-name-style
           'truncate-from-project)
     "truncate from project"
     :toggle (eq doom-modeline-buffer-file-name-style 'truncate-from-project))
    ("t w" (setq doom-modeline-buffer-file-name-style
           'truncate-with-project)
     "truncate with project"
     :toggle (eq doom-modeline-buffer-file-name-style 'truncate-with-project))
    ("t e" (setq doom-modeline-buffer-file-name-style
           'truncate-except-project)
     "truncate except project"
     :toggle
     (eq
      doom-modeline-buffer-file-name-style 'truncate-except-project))
    ("t r"
     (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
     "truncate upto root"
     :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-root))
    ("t a"
     (setq doom-modeline-buffer-file-name-style 'truncate-all)
     "truncate all"
     :toggle (eq doom-modeline-buffer-file-name-style 'truncate-all))
    ("t n"
     (setq doom-modeline-buffer-file-name-style 'truncate-nil)
     "truncate none"
     :toggle (eq doom-modeline-buffer-file-name-style 'truncate-nil))
    ("r f" (setq doom-modeline-buffer-file-name-style
           'relative-from-project)
     "relative from project"
     :toggle (eq doom-modeline-buffer-file-name-style 'relative-from-project))
    ("r t"
     (setq doom-modeline-buffer-file-name-style 'relative-to-project)
     "relative to project"
     :toggle (eq doom-modeline-buffer-file-name-style 'relative-to-project)))
   "Project Detection"
   (("p a"
     (setq doom-modeline-project-detection 'auto)
     "auto"
     :toggle (eq doom-modeline-project-detection 'auto))
    ("p f"
     (setq doom-modeline-project-detection 'ffip)
     "ffip"
     :toggle (eq doom-modeline-project-detection 'ffip))
    ("p i"
     (setq doom-modeline-project-detection 'projectile)
     "projectile"
     :toggle (eq doom-modeline-project-detection 'projectile))
    ("p p"
     (setq doom-modeline-project-detection 'project)
     "project"
     :toggle (eq doom-modeline-project-detection 'project))
    ("p n"
     (setq doom-modeline-project-detection nil)
     "disable"
     :toggle (eq doom-modeline-project-detection nil)))
   "Misc"
   (("n" (progn
       (message "Fetching GitHub notifications...")
       (run-with-timer
        300 nil #'doom-modeline--github-fetch-notifications)
       (browse-url "https://github.com/notifications"))
     "github notifications"
     :exit t)
    ("e" (cond
      ((bound-and-true-p flycheck-mode)
       (flycheck-list-errors))
      ((bound-and-true-p flymake-mode)
       (flymake-show-diagnostics-buffer)))
     "list errors"
     :exit t)
    ("w" (if (bound-and-true-p grip-mode)
         (grip-browse-preview)
       (message "Not in preview"))
     "browse preview"
     :exit t)
    ("z h"
     (set-from-minibuffer 'doom-modeline-height)
     "set height"
     :exit t)
    ("z w"
     (set-from-minibuffer 'doom-modeline-bar-width)
     "set bar width"
     :exit t)
    ("z g"
     (set-from-minibuffer 'doom-modeline-github-interval)
     "set github interval"
     :exit t)
    ("z n"
     (set-from-minibuffer 'doom-modeline-gnus-timer)
     "set gnus interval"
     :exit t)))))

(use-package
 hide-mode-line
 :hook
 (((treemacs-mode
    eshell-mode
    shell-mode
    term-mode
    vterm-mode
    embark-collect-mode
    lsp-ui-imenu-mode
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
     (setq-local composition-function-table
                 composition-ligature-table))))
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
         (61
          .
          ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
         (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
         (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
         (91 . ".\\(?:\\(|\\)[]|]?\\)")
         ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
         (94 . ".\\(?:\\(=\\)=?\\)")
         (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
         (119 . ".\\(?:\\(ww\\)w?\\)")
         (123 . ".\\(?:\\(|\\)[|}]?\\)")
         (124
          .
          ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
         (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
   (dolist (char-regexp alist)
     (set-char-table-range
      composition-ligature-table
      (car char-regexp)
      `([,(cdr char-regexp) 0 font-shape-gstring]))))
 (set-char-table-parent
  composition-ligature-table composition-function-table))


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

         flymake-diagnostics-buffer-mode
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
         "\\*lsp-help\\*$"
         "\\*lsp session\\*$"
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
     (when (and ;(called-interactively-p 'interactive)
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
