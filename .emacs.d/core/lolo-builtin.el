;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; Dired
(use-package
 dirvish
 :straight (:build t)
 :init (dirvish-override-dired-mode)
 :custom
 (dirvish-quick-access-entries
  '(("h" "~/" "Home") ("d" "~/Downloads/" "Downloads")))
 (dirvish-mode-line-format
  '(:left (sort file-time "" file-size symlink) :right (omit yank index)))
 (dirvish-attributes
  '(all-the-icons file-size collapse subtree-state vc-state git-msg))
 :config (dirvish-side-follow-mode)
 (setq
  dired-dwim-target t
  dired-recursive-copies 'always
  dired-recursive-deletes 'top
  delete-by-moving-to-trash t
  dirvish-preview-dispatchers (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers)
  dired-listing-switches "--all --human-readable --time-style=long-iso --group-directories-first -lv1"))

(dirvish-define-preview
 eza (file) "Use `eza' to generate directory preview."
 :require ("eza")
 (when (file-directory-p file)
   `(shell . ("eza" "--color=always" "-al" ,file))))

(add-to-list 'dirvish-preview-dispatchers 'eza)

(let ((lolo/file
       (lambda (path &optional dir)
         (expand-file-name path (or dir user-emacs-directory))))
      (lolo/dir
       (lambda (path &optional dir)
         (expand-file-name (file-name-as-directory path)
                           (or dir user-emacs-directory)))))
  (csetq
   image-dired-thumb-size
   150
   image-dired-dir
   (funcall lolo/dir "dired-img")
   image-dired-db-file
   (funcall lolo/file "dired-db.el")
   image-dired-gallery-dir
   (funcall lolo/dir "gallery")
   image-dired-temp-image-file
   (funcall lolo/file "temp-image" image-dired-dir)
   image-dired-temp-rotate-image-file
   (funcall lolo/file "temp-rotate-image" image-dired-dir)))

(use-package
 dired-rsync
 :if (executable-find "rsync")
 :defer t
 :straight (:build t))

;; ============================================================================
(use-package
 tramp
 :straight (tramp :type built-in :build t)
 :config
 (setq
  tramp-ssh-controlmaster-options nil
  tramp-verbose 0
  tramp-auto-save-directory (locate-user-emacs-file "tramp/")
  tramp-chunksize 2000)
 (add-to-list
  'backup-directory-alist ; deactivate auto-save with TRAMP
  (cons tramp-file-name-regexp nil)))

(add-to-list
 'tramp-methods
 '("yadm"
   (tramp-login-program "yadm")
   (tramp-login-args (("enter")))
   (tramp-login-env (("SHELL") ("/bin/sh")))
   (tramp-remote-shell "/bin/sh")
   (tramp-remote-shell-args ("-c"))))

(defun lolo/yadm ()
  "Manage my dotfiles through TRAMP."
  (interactive)
  (magit-status "/yadm::"))

;; ============================================================================
;; Compile
(use-package
 compile
 :defer t
 :straight (compile :type built-in)
 :hook (compilation-filter . colorize-compilation-buffer)
 :init (require 'ansi-color)
 (defun colorize-compilation-buffer ()
   (let ((inhibit-read-only t))
     (ansi-color-apply-on-region (point-min) (point-max))))
 :config (setq compilation-scroll-output t))

;; ============================================================================
(use-package
 tab-bar
 :defer t
 :straight (:type built-in)
 :custom
 (tab-bar-close-button-show nil)
 (tab-bar-new-button-show nil)
 (tab-bar-new-tab-choice "*dashboard*")
 :custom-face
 (tab-bar
  ((t
    (:background
     "#272C36"
     :foreground "#272C36"
     :box (:line-width (8 . 5) :style flat-button)))))
 :init
 (advice-add
  #'tab-new
  :after
  (lambda (&rest _)
    (when (y-or-n-p "Rename tab? ")
      (call-interactively #'tab-rename)))))

;; ============================================================================
(use-package
 bufler
 :straight (bufler :build t :files (:defaults (:exclude "helm-bufler.el")))
 :defer t)

(use-package
 helpful
 :straight (:build t)
 :after (counsel)
 :custom
 (counsel-describe-function-function #'helpful-callable)
 (counsel-describe-variable-function #'helpful-variable)
 :bind
 ([remap describe-function] . counsel-describe-function)
 ([remap describe-command] . helpful-command)
 ([remap describe-variable] . counsel-describe-variable)
 ([remap describe-key] . helpful-key))

;; ============================================================================
(use-package
 consult
 :straight (:build t)
 :bind
 ( ;; C-c bindings in `mode-specific-map'
  ("C-c M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c m" . consult-man)
  ("C-c i" . consult-info)
  ("C-c r" . consult-ripgrep)
  ("C-c T" . consult-theme)
  ("C-." . consult-imenu)
  ("C-c c e" . consult-colors-emacs)
  ("C-c c w" . consult-colors-web)
  ("C-c c f" . describe-face)
  ("C-c c t" . consult-theme)
  ([remap Info-search] . consult-info)
  ([remap isearch-forward] . consult-line)
  ([remap recentf-open-files] . consult-recent-file)
  ;; C-x bindings in `ctl-x-map'
  ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
  ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
  ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
  ;; Custom M-# bindings for fast register access
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
  ("C-M-#" . consult-register)
  ;; Other custom bindings
  ("M-y" . consult-yank-pop) ;; orig. yank-pop
  ;; M-g bindings in `goto-map'
  ("M-g e" . consult-compile-error)
  ("M-g g" . consult-goto-line) ;; orig. goto-line
  ("M-g M-g" . consult-goto-line) ;; orig. goto-line
  ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; M-s bindings in `search-map'
  ("M-s d" . consult-find)
  ("M-s D" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Isearch integration
  ("M-s e" . consult-isearch-history)
  :map
  isearch-mode-map
  ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
  ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
  ;; Minibuffer history
  :map
  minibuffer-local-map
  ("C-s" .
   (lambda ()
     "Insert the selected region or current symbol at point."
     (interactive)
     (insert
      (with-current-buffer (window-buffer (minibuffer-selected-window))
        (or (and transient-mark-mode
                 mark-active
                 (/= (point) (mark))
                 (buffer-substring-no-properties (point) (mark)))
            (thing-at-point 'symbol t) "")))))
  ("M-s" . consult-history) ;; orig. next-matching-history-element
  ("M-r" . consult-history)) ;; orig. previous-matching-history-element
 :hook (completion-list-mode . consult-preview-at-point-mode)
 :init
 (setq
  register-preview-delay 0.5
  register-preview-function #'consult-register-format)
 (advice-add #'register-preview :override #'consult-register-window)
 ;; Use Consult to select xref locations with preview
 (with-eval-after-load 'xref
   (setq
    xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref))
 ;; More utils
 (defvar consult-colors-history nil
   "History for `consult-colors-emacs' and `consult-colors-web'.")
 ;; No longer preloaded in Emacs 28.
 (autoload 'list-colors-duplicates "facemenu")
 ;; No preloaded in consult.el
 (autoload 'consult--read "consult")

 (defun consult-colors-emacs (color)
   "Show a list of all supported colors for a particular frame.

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
   (interactive (list
                 (consult--read
                  (list-colors-duplicates (defined-colors))
                  :prompt "Emacs color: "
                  :require-match t
                  :category 'color
                  :history '(:input consult-colors-history))))
   (insert color))

 (defun consult-colors--web-list nil
   "Return list of CSS colors for `counsult-colors-web'."
   (require 'shr-color)
   (sort
    (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist))
    #'string-lessp))

 (defun consult-colors-web (color)
   "Show a list of all CSS colors.\

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
   (interactive (list
                 (consult--read
                  (consult-colors--web-list)
                  :prompt "Color: "
                  :require-match t
                  :category 'color
                  :history '(:input consult-colors-history))))
   (insert color))
 :config (setq consult-preview-key '(:debounce 1.0 any))
 (consult-customize
  consult-goto-line
  consult-theme
  :preview-key '(:debounce 0.5 any))

 (setq consult-narrow-key "<") ;; "C-+"

 (define-key
  consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

(use-package
 consult-flyspell
 :straight (:build t)
 :bind ("M-g s" . consult-flyspell))

(use-package
 consult-yasnippet
 :straight (:build t)
 :bind ("M-g y" . consult-yasnippet))

(use-package
 embark
 :straight (:build t)
 :bind
 (("s-." . embark-act)
  ("C-s-." . embark-act)
  ("M-." . embark-dwim) ; overrides `xref-find-definitions'
  ([remap describe-bindings] . embark-bindings)
  :map minibuffer-local-map ("M-." . my-embark-preview))
 :init
 ;; Optionally replace the key help with a completing-read interface
 (setq prefix-help-command #'embark-prefix-help-command)
 :config
 ;; Manual preview for non-Consult commands using Embark
 (defun my-embark-preview ()
   "Previews candidate in vertico buffer, unless it's a consult command."
   (interactive)
   (unless (bound-and-true-p consult--preview-function)
     (save-selected-window
       (let ((embark-quit-after-action nil))
         (embark-dwim)))))

 ;; Hide the mode line of the Embark live/completions buffers
 (add-to-list
  'display-buffer-alist
  '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
    nil
    (window-parameters (mode-line-format . none))))

 (with-eval-after-load 'which-key
   (defun embark-which-key-indicator ()
     "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
     (lambda (&optional keymap targets prefix)
       (if (null keymap)
           (which-key--hide-popup-ignore-command)
         (which-key--show-keymap
          (if (eq (plist-get (car targets) :type) 'embark-become)
              "Become"
            (format "Act on %s '%s'%s"
                    (plist-get (car targets) :type)
                    (embark--truncate-target (plist-get (car targets) :target))
                    (if (cdr targets)
                        "…"
                      "")))
          (if prefix
              (pcase (lookup-key keymap prefix 'accept-default)
                ((and (pred keymapp) km) km)
                (_ (key-binding prefix 'accept-default)))
            keymap)
          nil nil t
          (lambda (binding)
            (not (string-suffix-p "-argument" (cdr binding))))))))

   (setq embark-indicators
         '(embark-which-key-indicator
           embark-highlight-indicator embark-isearch-highlight-indicator))

   (defun embark-hide-which-key-indicator (fn &rest args)
     "Hide the which-key indicator immediately when using the completing-read prompter."
     (which-key--hide-popup-ignore-command)
     (let ((embark-indicators
            (remq #'embark-which-key-indicator embark-indicators)))
       (apply fn args)))

   (advice-add
    #'embark-completing-read-prompter
    :around #'embark-hide-which-key-indicator)))

(use-package
 embark-consult
 :straight (:build t)
 :bind (:map minibuffer-mode-map ("C-c C-o" . embark-export))
 :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================================
(use-package
 undo-tree
 :straight (:build t)
 :defer t
 :diminish undo-tree-mode
 :custom (undo-tree-visualizer-diff t)
 (undo-tree-history-directory-alist
  `(("." . ,(expand-file-name ".backup" lolo-savefile-dir))))
 (undo-tree-visualizer-timestamps t)
 :config (global-undo-tree-mode t))

;; ============================================================================
(use-package
 color-rg
 :straight nil
 :load-path (lambda () (expand-file-name "site-elisp/color-rg" lolo-dir)))

(provide 'lolo-builtin)
;;; lolo-builtin.el ends here
