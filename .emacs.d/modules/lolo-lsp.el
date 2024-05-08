;;; lolo-lsp.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'lolo-custom))

(setq read-process-output-max (* 1024 1024)) ; 1MB
(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-treemacs
  :ensure t)

;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode#supported-languages
(use-package
  lsp-mode
  :diminish
  :defines (lsp-diagnostics-disabled-modes lsp-clients-python-library-directories)
  :autoload lsp-enable-which-key-integration
  :commands (lsp-format-buffer lsp-organize-imports)
  :hook
  ((prog-mode
    .
    (lambda ()
      (unless (derived-mode-p
               'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
        (lsp-deferred))))
   ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
   (lsp-mode
    .
    (lambda ()
      ;; Integrate `which-key'
      (lsp-enable-which-key-integration)

      ;; Format and organize imports
      (when (and lolo-lsp-format-on-save
                 (not
                  (apply #'derived-mode-p lolo-lsp-format-on-save-ignore-modes)))
        (add-hook 'before-save-hook #'lsp-format-buffer t t)
        (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
  :bind
  (:map
   lsp-mode-map
   ("C-c C-d" . lsp-describe-thing-at-point)
   ([remap xref-find-definitions] . lsp-find-definition)
   ([remap xref-find-references] . lsp-find-references))
  :init
  (setq
   lsp-keymap-prefix "C-c l"
   lsp-keep-workspace-alive nil
   lsp-signature-auto-activate nil
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   lsp-modeline-workspace-status-enable nil

   lsp-semantic-tokens-enable t
   lsp-progress-spinner-type 'progress-bar-filled

   lsp-enable-file-watchers nil
   lsp-enable-folding nil
   lsp-enable-symbol-highlighting nil
   lsp-enable-text-document-color nil

   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil

   ;; For diagnostics
   lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

   ;; For clients
   lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  :config
  (use-package
    consult-lsp
    :bind (:map lsp-mode-map ("C-M-." . consult-lsp-symbols)))

  (with-no-warnings
    ;; Disable `lsp-mode' in `git-timemachine-mode'
    (defun my-lsp--init-if-visible (fn &rest args)
      (unless (bound-and-true-p git-timemachine-mode)
        (apply fn args)))
    (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

    ;; Enable `lsp-mode' in sh/bash/zsh
    (defun my-lsp-bash-check-sh-shell (&rest _)
      (and (memq major-mode '(sh-mode bash-ts-mode))
           (memq sh-shell '(sh bash zsh))))
    (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell)
    (add-to-list 'lsp-language-id-configuration '(bash-ts-mode . "shellscript"))

    ;; Display icons
    (when (icons-displayable-p)
      (defun my-lsp-icons-get-symbol-kind (fn &rest args)
        (and (icons-displayable-p) (apply fn args)))
      (advice-add
       #'lsp-icons-get-by-symbol-kind
       :around #'my-lsp-icons-get-symbol-kind)

      ;; For `lsp-headerline'
      (defun my-lsp-icons-get-by-file-ext (fn &rest args)
        (and (icons-displayable-p) (apply fn args)))
      (advice-add
       #'lsp-icons-get-by-file-ext
       :around #'my-lsp-icons-get-by-file-ext)

      (defun my-lsp-icons-get-by-file-ext (file-ext &optional feature)
        (when (and file-ext (lsp-icons--enabled-for-feature feature))
          (nerd-icons-icon-for-extension file-ext)))
      (advice-add
       #'lsp-icons-get-by-file-ext
       :override #'my-lsp-icons-get-by-file-ext)

      (defvar lsp-symbol-alist
        '((misc
           nerd-icons-codicon
           "nf-cod-symbol_namespace"
           :face font-lock-warning-face)
          (document
           nerd-icons-codicon
           "nf-cod-symbol_file"
           :face font-lock-string-face)
          (namespace
           nerd-icons-codicon
           "nf-cod-symbol_namespace"
           :face font-lock-type-face)
          (string
           nerd-icons-codicon
           "nf-cod-symbol_string"
           :face font-lock-doc-face)
          (boolean-data
           nerd-icons-codicon
           "nf-cod-symbol_boolean"
           :face font-lock-builtin-face)
          (numeric
           nerd-icons-codicon
           "nf-cod-symbol_numeric"
           :face font-lock-builtin-face)
          (method
           nerd-icons-codicon
           "nf-cod-symbol_method"
           :face font-lock-function-name-face)
          (field
           nerd-icons-codicon
           "nf-cod-symbol_field"
           :face font-lock-variable-name-face)
          (localvariable
           nerd-icons-codicon
           "nf-cod-symbol_variable"
           :face font-lock-variable-name-face)
          (class
           nerd-icons-codicon
           "nf-cod-symbol_class"
           :face font-lock-type-face)
          (interface
           nerd-icons-codicon
           "nf-cod-symbol_interface"
           :face font-lock-type-face)
          (property
           nerd-icons-codicon
           "nf-cod-symbol_property"
           :face font-lock-variable-name-face)
          (indexer
           nerd-icons-codicon
           "nf-cod-symbol_enum"
           :face font-lock-builtin-face)
          (enumerator
           nerd-icons-codicon
           "nf-cod-symbol_enum"
           :face font-lock-builtin-face)
          (enumitem
           nerd-icons-codicon
           "nf-cod-symbol_enum_member"
           :face font-lock-builtin-face)
          (constant
           nerd-icons-codicon
           "nf-cod-symbol_constant"
           :face font-lock-constant-face)
          (structure
           nerd-icons-codicon
           "nf-cod-symbol_structure"
           :face font-lock-variable-name-face)
          (event
           nerd-icons-codicon
           "nf-cod-symbol_event"
           :face font-lock-warning-face)
          (operator
           nerd-icons-codicon
           "nf-cod-symbol_operator"
           :face font-lock-comment-delimiter-face)
          (template
           nerd-icons-codicon
           "nf-cod-symbol_snippet"
           :face font-lock-type-face)))

      (defun my-lsp-icons-get-by-symbol-kind (kind &optional feature)
        (when (and kind (lsp-icons--enabled-for-feature feature))
          (let* ((icon
                  (cdr
                   (assoc
                    (lsp-treemacs-symbol-kind->icon kind) lsp-symbol-alist)))
                 (args (cdr icon)))
            (apply (car icon) args))))
      (advice-add
       #'lsp-icons-get-by-symbol-kind
       :override #'my-lsp-icons-get-by-symbol-kind)

      (setq lsp-headerline-arrow
            (nerd-icons-octicon
             "nf-oct-chevron_right"
             :face 'lsp-headerline-breadcrumb-separator-face)))))

(use-package
  lsp-ui
  :custom-face (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :pretty-hydra
  ((:title
    (pretty-hydra-title "LSP UI" 'faicon "nf-fa-rocket" :face 'nerd-icons-green)
    :color amaranth
    :quit-key ("q" "C-g"))
   ("Doc" (("d e" (progn
                    (lsp-ui-doc-enable (not lsp-ui-doc-mode))
                    (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
            "enable"
            :toggle lsp-ui-doc-mode)
           ("d s"
            (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
            "signature"
            :toggle lsp-ui-doc-include-signature)
           ("d t"
            (setq lsp-ui-doc-position 'top)
            "top"
            :toggle (eq lsp-ui-doc-position 'top))
           ("d b"
            (setq lsp-ui-doc-position 'bottom)
            "bottom"
            :toggle (eq lsp-ui-doc-position 'bottom))
           ("d p"
            (setq lsp-ui-doc-position 'at-point)
            "at point"
            :toggle (eq lsp-ui-doc-position 'at-point))
           ("d h"
            (setq lsp-ui-doc-header (not lsp-ui-doc-header))
            "header"
            :toggle lsp-ui-doc-header)
           ("d f"
            (setq lsp-ui-doc-alignment 'frame)
            "align frame"
            :toggle (eq lsp-ui-doc-alignment 'frame))
           ("d w"
            (setq lsp-ui-doc-alignment 'window)
            "align window"
            :toggle (eq lsp-ui-doc-alignment 'window)))
    "Sideline"
    (("s e" (progn
              (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
              (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
      "enable"
      :toggle lsp-ui-sideline-mode)
     ("s h"
      (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
      "hover"
      :toggle lsp-ui-sideline-show-hover)
     ("s d" (setq lsp-ui-sideline-show-diagnostics
                  (not lsp-ui-sideline-show-diagnostics))
      "diagnostics"
      :toggle lsp-ui-sideline-show-diagnostics)
     ("s s"
      (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
      "symbol"
      :toggle lsp-ui-sideline-show-symbol)
     ("s c" (setq lsp-ui-sideline-show-code-actions
                  (not lsp-ui-sideline-show-code-actions))
      "code actions"
      :toggle lsp-ui-sideline-show-code-actions)
     ("s i" (setq lsp-ui-sideline-ignore-duplicate
                  (not lsp-ui-sideline-ignore-duplicate))
      "ignore duplicate"
      :toggle lsp-ui-sideline-ignore-duplicate))
    "Action"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→")
     ("C-a" mwim-beginning-of-code-or-line nil)
     ("C-e" mwim-end-of-code-or-line nil)
     ("C-b" backward-char nil)
     ("C-n" next-line nil)
     ("C-p" previous-line nil)
     ("C-f" forward-char nil)
     ("M-b" backward-word nil)
     ("M-f" forward-word nil)
     ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
  :bind
  (("C-c u" . lsp-ui-imenu)
   :map
   lsp-ui-mode-map
   ("M-<f6>" . lsp-ui-hydra/body)
   ("s-<return>" . lsp-ui-sideline-apply-code-actions)
   ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
   ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq
   lsp-ui-sideline-show-diagnostics nil
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-doc-delay 0.1
   lsp-ui-doc-show-with-cursor (not (display-graphic-p))
   lsp-ui-imenu-auto-refresh 'after-save
   lsp-ui-imenu-colors
   `(,(face-foreground 'font-lock-keyword-face)
     ,(face-foreground 'font-lock-string-face)
     ,(face-foreground 'font-lock-constant-face)
     ,(face-foreground 'font-lock-variable-name-face)))
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
          (if (facep 'posframe-border)
              (face-background 'posframe-border nil t)
            (face-background 'region nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)
  :config
  (with-no-warnings
    ;; Display peek in child frame if possible
    ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
    (defvar lsp-ui-peek--buffer nil)
    (defun lsp-ui-peek--peek-display (fn src1 src2)
      (if (childframe-workable-p)
          (-let* ((win-width (frame-width))
                  (lsp-ui-peek-list-width (/ (frame-width) 2))
                  (string
                   (-some-->
                       (-zip-fill "" src1 src2)
                     (--map (lsp-ui-peek--adjust win-width it) it)
                     (-map-indexed 'lsp-ui-peek--make-line it)
                     (-concat it (lsp-ui-peek--make-footer)))))
            (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
            (posframe-show
             lsp-ui-peek--buffer
             :string (mapconcat 'identity string "")
             :min-width (frame-width)
             :internal-border-color (face-background 'posframe-border nil t)
             :internal-border-width 1
             :poshandler #'posframe-poshandler-frame-center))
        (funcall fn src1 src2)))
    (defun lsp-ui-peek--peek-destroy (fn)
      (if (childframe-workable-p)
          (progn
            (when (bufferp lsp-ui-peek--buffer)
              (posframe-hide lsp-ui-peek--buffer))
            (setq lsp-ui-peek--last-xref nil))
        (funcall fn)))
    (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
    (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

    ;; Handle docs
    (defun my-lsp-ui-doc--handle-hr-lines nil
      (let (bolp
            next
            before
            after)
        (goto-char 1)
        (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
          (when (get-text-property next 'markdown-hr)
            (goto-char next)
            (setq
             bolp (bolp)
             before (char-before))
            (delete-region
             (point)
             (save-excursion
               (forward-visible-line 1)
               (point)))
            (setq after (char-after (1+ (point))))
            (insert
             (concat
              (and bolp
                   (not (equal before ?\n))
                   (propertize "\n" 'face '(:height 0.5)))
              (propertize "\n" 'face '(:height 0.5))
              (propertize " "
                          ;; :align-to is added with lsp-ui-doc--fix-hr-props
                          'display
                          '(space :height (1))
                          'lsp-ui-doc--replace-hr
                          t
                          'face
                          `(:background
                            ,(face-foreground 'font-lock-comment-face nil t)))
              ;; :align-to is added here too
              (propertize " " 'display '(space :height (1)))
              (and (not (equal after ?\n))
                   (propertize " \n" 'face '(:height 0.5)))))))))
    (advice-add
     #'lsp-ui-doc--handle-hr-lines
     :override #'my-lsp-ui-doc--handle-hr-lines)))

;; Python
(use-package
  lsp-pyright
  :preface
  ;; Use yapf to format
  (defun lsp-pyright-format-buffer ()
    (interactive)
    (when (and (executable-find "yapf") buffer-file-name)
      (call-process "yapf" nil nil nil "-i" buffer-file-name)))
  :hook
  (((python-mode python-ts-mode)
    .
    (lambda ()
      (require 'lsp-pyright)
      (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t))))
  :init
  (when (executable-find "python3")
    (setq lsp-pyright-python-executable-cmd "python3")))

;; C/C++/Objective-C
(use-package
  ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
  :config
  (with-no-warnings
    (cl-defmethod my-lsp-execute-command
      ((_server (eql ccls)) (command (eql ccls.xref)) arguments)
      (when-let ((xrefs
                  (lsp--locations-to-xref-items
                   (lsp--send-execute-command (symbol-name command) arguments))))
        (xref--show-xrefs xrefs nil)))
    (advice-add #'lsp-execute-command :override #'my-lsp-execute-command)))


(cl-defmacro
    lsp-org-babel-enable
    (lang)
  "Support LANG in org source code block."
  (cl-check-type lang string)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (setq buffer-file-name
            (or (->> info caddr (alist-get :file)) "org-src-babel.tmp"))
         (when (fboundp 'lsp-deferred)
           ;; Avoid headerline conflicts
           (setq-local lsp-headerline-breadcrumb-enable nil)
           (lsp-deferred)))
       (put
        ',intern-pre 'function-documentation
        (format "Enable `%s' in the buffer of org source block (%s)."
                lolo-lsp
                (upcase ,lang)))

       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put
            ',edit-pre 'function-documentation
            (format "Prepare local buffer environment for org source block (%s)."
                    (upcase ,lang))))))))

(defconst org-babel-lang-list
  '("go"
    "python"
    "ipython"
    "js"
    "css"
    "sass"
    "c"
    "rust"
    "java"
    "cpp"
    "c++"
    "shell")
  "The supported programming languages for interactive Babel.")
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))


(provide 'lolo-lsp)
;;; lolo-lsp.el ends here
