;;; lolo-editor.el --------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq reuire-final-newline t)
(delete-selection-mode t)

(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; start tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; smart pairing for all
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entrire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)

(define-key prog-mode-map (kbd "M-(") (lolo-wrap-with "("))
(define-key prog-mode-map (kbd "M-\"") (lolo-wrap-with "\""))

(setq blink-matching-paren nil)

(require 'diminish)
(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq save-place-file (expand-file-name "saveplace" lolo-savefile-dir))
(save-place-mode 1)

(require 'savehist)
(setq savehist-additional-variables
    '(search-ring regexp-search-ring)
    savehist-autosave-interval 60
    savehist-file (expand-file-name "savehist" lolo-savefile-dir))
(savehist-mode +1)

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" lolo-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)

(defun lolo-recentf-exclude-p (file)
    "A predicate to decide whether to exclude FILE from recentf."
    (let ((file-dir (file-truename (file-name-directory file))))
        (cl-some (lambda (dir)
                    (string-prefix-p dir file-dir))
                 (mapcar 'file-truename (list lolo-savefile-dir package-user-dir)))))

(add-to-list 'recentf-exclude 'lolo-recentf-exclude-p)
(recentf-mode +1)

;; (require 'windmove)
;; (windmove-default-keybindings)

(require 'super-save)
(add-to-list 'super-save-triggers 'ace-window)
(super-save-mode +1)
(diminish 'super-save-mode)

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
    "Set buffer major mode according to `auto-mode-alist'."
    (let* ((name (buffer-name buffer))
           (mode (assoc-default name auto-mode-alist 'string-match)))
          (when (and mode (consp mode))
           (setq mode (car mode)))
          (with-current-buffer buffer (if mode (funcall mode)))))

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

(require 'rect)
(require 'crux)
(crux-with-region-or-line kill-region)

(require 'tramp)
(setq tramp-default-method "ssh")

(set-default 'imenu-auto-rescan t)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(defun lolo-enable-flyspell ()
    "Enable common `flyspell-mode' if `lolo-flyspell' is not nil."
    (when (and lolo-flyspell (executable-find ispell-program-name))
        (flyspell-mode +1)))

(defun lolo-cleanup-maybe ()
    "Invoke `whitespace-cleanup'"
    (when lolo-clean-whitespace-on-save
        (whitespace-cleanup)))

(defun lolo-enable-whitespace ()
    "Enable `whitespace-mode' "
    (when lolo-whitespace
        (add-hook 'before-save-hook 'lolo-cleanup-maybe nil t)
        (whitespace-mode +1)))

(add-hook 'text-mode-hook 'lolo-enable-flyspell)
(add-hook 'text-mode-hook 'lolo-enable-whitespace)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(require 'expand-region)

;; bookmark
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" lolo-savefile-dir)
      bookmark-save-flag 1)

;; projectile is a project management mode
(require 'projectile)
(setq projectile-cache-file (expand-file-name "projectile.cache" lolo-savefile-dir))
(projectile-mode t)

;; avy allows us to effectively navigate to visible things
(require 'avy)
(setq avy-background t)
(setq avy-style 'at-full)

;; anzu-mode enhance isearch & query-relace
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)


(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; smarter kill-ring navigation
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(require 'tabify)
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) lolo-yank-indent-threshold)
      (indent-region beg end nil)))

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

(advise-commands "indent" (yank yank-pop) after
  "If current mode is one of `lolo-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode lolo-indent-sensitive-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)
(diminish 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" lolo-savefile-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" lolo-savefile-dir))

;; Compilation from Emacs
(defun lolo-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
      compilation-scroll-output 'first-error ; Automatically scroll to first
      )

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'lolo-colorize-compilation-buffer)

;; enable Lolo's keybindings
(lolo-mode t)

;; supercharge your undo/redo with undo-tree
(require 'undo-tree)
;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; enable winner-mode to manage window configurations
(winner-mode +1)

;; diff-hl
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; operate-on-number
(require 'operate-on-number)
(require 'smartrep)

(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line."
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn))) files)))

;; use settings from .editorconfig file when present
(require 'editorconfig)
(editorconfig-mode 1)
(diminish 'editorconfig-mode)

(provide 'lolo-editor)
;;; lolo-editor.el ends here
