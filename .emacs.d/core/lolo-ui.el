;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package
 dashboard
 :straight (:build t)
 :ensure t
 :after all-the-icons
 :config
 (setq
  dashboard-banner-logo-title "Zrik Vanilla Emacs"
  dashboard-startup-banner 'logo
  dashboard-center-content t
  dashboard-show-shortcuts t
  dashboard-set-navigator t
  dashboard-set-heading-icons t
  dashboard-set-file-icons t
  initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
  dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
 (setq dashboard-navigator-buttons
       `(((,(all-the-icons-faicon "level-up" :height 1.1 :v-adjust 0.0)
           "Update Packages" ""
           (lambda (&rest _)
             (progn
               (require 'straight)
               (straight-pull-all)
               (straight-rebuild-all)))))))

 (setq dashboard-items '((recents . 15) (agenda . 10) (projects . 10)))
 (dashboard-setup-startup-hook)
 :init (add-hook 'after-init-hook 'dashboard-refresh-buffer))

;; ============================================================================
(use-package
 git-gutter-fringe
 :straight (:build t)
 :hook
 ((prog-mode . git-gutter-mode)
  (org-mode . git-gutter-mode)
  (markdown-mode . git-gutter-mode)
  (latex-mode . git-gutter-mode)))

;; ============================================================================
(use-package all-the-icons :defer t :straight (:build t))

(defun prog-mode-set-symbols-alist ()
  (setq prettify-symbols-alist '(("lambda" . ?λ)))
  (prettify-symbols-mode 1))

(add-hook 'prog-mode-hook #'prog-mode-set-symbols-alist)

;; ============================================================================
(use-package
 ligature
 :straight
 (ligature :type git :host github :repo "mickeynp/ligature.el" :build t)
 :config
 (ligature-set-ligatures 't '("www"))
 (ligature-set-ligatures
  '(eww-mode org-mode elfeed-show-mode) '("ff" "fi" "ffi"))
 (ligature-set-ligatures
  'prog-mode
  '("|||>"
    "<|||"
    "<==>"
    "<!--"
    "####"
    "~~>"
    "***"
    "||="
    "||>"
    ":::"
    "::="
    "=:="
    "==="
    "==>"
    "=!="
    "=>>"
    "=<<"
    "=/="
    "!=="
    "!!."
    ">=>"
    ">>="
    ">>>"
    ">>-"
    ">->"
    "->>"
    "-->"
    "---"
    "-<<"
    "<~~"
    "<~>"
    "<*>"
    "<||"
    "<|>"
    "<$>"
    "<=="
    "<=>"
    "<=<"
    "<->"
    "<--"
    "<-<"
    "<<="
    "<<-"
    "<<<"
    "<+>"
    "</>"
    "###"
    "#_("
    "..<"
    "..."
    "+++"
    "/=="
    "///"
    "_|_"
    "www"
    "&&"
    "^="
    "~~"
    "~@"
    "~="
    "~>"
    "~-"
    "**"
    "*>"
    "*/"
    "||"
    "|}"
    "|]"
    "|="
    "|>"
    "|-"
    "{|"
    "[|"
    "]#"
    "::"
    ":="
    ":>"
    ":<"
    "$>"
    "=="
    "=>"
    "!="
    "!!"
    ">:"
    ">="
    ">>"
    ">-"
    "-~"
    "-|"
    "->"
    "--"
    "-<"
    "<~"
    "<*"
    "<|"
    "<:"
    "<$"
    "<="
    "<>"
    "<-"
    "<<"
    "<+"
    "</"
    "#{"
    "#["
    "#:"
    "#="
    "#!"
    "##"
    "#("
    "#?"
    "#_"
    "%%"
    ".="
    ".-"
    ".."
    ".?"
    "+>"
    "++"
    "?:"
    "?="
    "?."
    "??"
    ";;"
    "/*"
    "/="
    "/>"
    "//"
    "__"
    "~~"
    "(*"
    "*)"
    "\\\\"
    "://"))
 (global-ligature-mode t))

;; ============================================================================
(use-package
 doom-modeline
 :straight (:build t)
 :defer t
 :init
 (doom-modeline-mode 1)
 (setq find-file-visit-truename t)
 :custom
 (doom-modeline-support-imenu t)
 (doom-modeline-bar-width 4)
 (doom-modeline-hud nil)
 (doom-modeline-window-width-limit 85)
 (doom-modeline-project-detection 'auto)
 (doom-modeline-height 36)
 (doom-modeline-enable-word-count t)
 (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
 (doom-modeline-4e t)
 (doom-modeline-env-version t)
 (doom-modeline-buffer-file-name-style 'truncate-upto-project)
 (inhibit-compacting-font-caches t)
 (find-file-visit-truename t)
 :config
 (custom-set-faces
  '(mode-line ((t (:family "Terminess Nerd Font" :height 1.0))))
  '(mode-line-active
    ((t (:family "Terminess Nerd Font" :height 1.0)))) ; For 29+
  '(mode-line-inactive ((t (:family "Terminess Nerd Font" :height 1.0))))))

(use-package
 hide-mode-line
 :straight (:build t)
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
(use-package
 minions
 :straight (:build t)
 :hook (doom-modeline-mode . minions-mode))

;; Child frame
(use-package
 posframe
 :straight (:build t)
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
   (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

   (defun posframe-poshandler-frame-center-near-bottom (info)
     (cons
      (/ (- (plist-get info :parent-frame-width)
            (plist-get info :posframe-width))
         2)
      (/ (+ (plist-get info :parent-frame-height)
            (* 2 (plist-get info :font-height)))
         2)))))

;; Enforce rules for popups
(use-package
 popper
 :straight (:build t)
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
                        'eshell-mode 'shell-mode 'term-mode 'vterm-mode)))
                    (window-live-p window))
           (delete-window window)))))
   (advice-add #'keyboard-quit :before #'popper-close-window-hack)))

;; ============================================================================
(use-package
 valign
 :defer t
 :straight (:build t)
 :after (org markdown-mode)
 :custom ((valign-fancy-bar t)))

;; ============================================================================
(use-package
 secret-mode
 :defer t
 :straight
 (secret-mode :build t :type git :host github :repo "bkaestner/secret-mode.el"))

;; ============================================================================
(use-package
 solaire-mode
 :defer t
 :straight (:build t)
 :init (solaire-global-mode +1))

;; ============================================================================
;; (use-package gruber-darker-theme :defer t :straight (:build t))

;; (use-package
;;  ef-themes
;;  :straight (:build t)
;;  :demand t
;;  :config
;;  (setq
;;   ef-themes-variable-pitch-ui t
;;   ef-themes-mixed-fonts t
;;   ef-themes-headings ; read the manual's entry of the doc string
;;   '((0 . (variable-pitch light 1.9))
;;     (1 . (variable-pitch light 1.8))
;;     (2 . (variable-pitch regular 1.7))
;;     (3 . (variable-pitch regular 1.6))
;;     (4 . (variable-pitch regular 1.5))
;;     (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
;;     (6 . (variable-pitch 1.3))
;;     (7 . (variable-pitch 1.2))
;;     (agenda-date . (semilight 1.5))
;;     (agenda-structure . (variable-pitch light 1.9))
;;     (t . (variable-pitch 1.1)))))

(use-package
 modus-themes
 :defer t
 :straight (:build t)
 :config
 (setq
  modus-themes-custom-auto-reload nil
  modus-themes-mixed-fonts t
  modus-themes-variable-pitch-ui t
  modus-themes-italic-constructs t
  modus-themes-bold-constructs nil
  modus-themes-completions '((t . (extrabold)))
  modus-themes-prompts '(extrabold)
  modus-themes-headings
  '((agenda-structure . (variable-pitch light 2.2))
    (agenda-date . (variable-pitch regular 1.3))
    (t . (regular 1.15))))
 (setq modus-themes-common-palette-overrides nil))

;; (use-package standard-themes
;;   :defer t
;;   :straight (:build t)
;;   :config
;;   (setq standard-themes-bold-constructs t
;;         standard-themes-italic-constructs t
;;         standard-themes-mixed-fonts t
;;         standard-themes-variable-pitch-ui t
;;         standard-themes-mode-line-accented nil
;;         ;; Accepts a symbol value
;;         standard-themes-fringes 'subtle
;;         ;; The following accept lists of properties
;;         standard-themes-links nil
;;         standard-themes-region nil
;;         standard-themes-prompts nil
;;         ;; more complex alist to set weight, height, and optional
;;         ;; `variable-pitch' per heading level (t is for any level not
;;         ;; specified)
;;         standard-themes-headings
;;         '((0 . (variable-pitch light 1.9))
;;           (1 . (variable-pitch light 1.8))
;;           (2 . (variable-pitch light 1.7))
;;           (3 . (variable-pitch semilight 1.6))
;;           (4 . (variable-pitch semilight 1.5))
;;           (5 . (variable-pitch 1.4))
;;           (6 . (variable-pitch 1.3))
;;           (7 . (variable-pitch 1.2))
;;           (agenda-date . (1.3))
;;           (agenda-structure . (variable-pitch light 1.8))
;;           (t . (variable-pitch 1.1)))))

(let ((value lolo-var-theme))
  (cond
   ((eql value 'gruber-darker)
    (load-theme 'gruber-darker :no-confirm))
   ((eql value 'ef)
    (load-theme 'ef-trio-light :no-confirm))
   ((eql value 'modus)
    (load-theme 'modus-operandi-tritanopia :no-confirm))
   ((eql value 'standard)
    (load-theme 'standard-light :no-confirm))))

;; ============================================================================
(use-package
 rainbow-delimiters
 :straight (:build t)
 :defer t
 :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================================
(use-package
 page-break-lines
 :straight (:build t)
 :diminish
 :init (global-page-break-lines-mode))

;; ============================================================================
(use-package
 info-colors
 :straight (:build t)
 :commands info-colors-fnontify-node
 :hook (Info-selection . info-colors-fontify-node)
 :hook (Info-mode . mixed-pitch-mode))

;; ============================================================================
(use-package
 winner
 :custom
 (winner-boring-buffers
  '("*Completions*"
    "*Compile-Log*"
    "*inferior-lisp*"
    "*Fuzzy Completions*"
    "*Apropos*"
    "*Help*"
    "*cvs*"
    "*Buffer List*"
    "*Ibuffer*"
    "*esh command on file*"))
 :config (winner-mode 1))

(provide 'lolo-ui)
;;; lolo-ui.el ends here
