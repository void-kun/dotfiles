;;; init-core.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; Mapping file to mode.
(defvar lolo-auto-install-alist
  '(("\\.adoc\\'" adoc-mode adoc-mode)
    ("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cljc\\'" clojure-mode clojurec-mode)
    ("\\.cljs\\'" clojure-mode clojurescript-mode)
    ("\\.edn\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.jl\\'" julia-mode julia-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rkt\\'" racket-mode racket-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

(when (package-installed-p 'adoc-mode)
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
  (add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; ============================================================================
;; Move around text.
(use-package
 avy
 :defer t
 :bind (("C-z c" . avy-goto-char-timer) ("C-z l" . avy-goto-line))
 :custom (avy-timeout-seconds 0.3) (avy-style 'pre)
 :custom-face
 (avy-lead-face
  ((t (:background "#51afef" :foreground "#870000" :weight bold))))) ;

;; ============================================================================
;; a Collection of Ridiculously Useful eXtensions for Emacs
(use-package
 crux
 :bind
 (("C-a" . crux-move-beginning-of-line)
  ("C-x 4 t" . crux-transpose-windows)
  ("C-x K" . crux-kill-other-buffers)
  ("C-k" . crux-smart-kill-line))
 :config
 (crux-with-region-or-buffer indent-region)
 (crux-with-region-or-buffer untabify)
 (crux-with-region-or-point-to-eol kill-ring-save)
 (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

;; ============================================================================
;; a generic completion mechanism for Emacs
(use-package
 ivy
 :diminish
 :init
 (use-package amx :defer t)
 (use-package counsel :diminish :config (counsel-mode 1))
 (use-package swiper :defer t)
 (ivy-mode 1)
 :bind
 (("C-s" . swiper-isearch)
  ("C-z s" . counsel-rg)
  ("C-z b" . counsel-buffer-or-recentf)
  ("C-z C-b" . counsel-ibuffer)
  ("M-y" . counsel-yank-pop)
  (:map ivy-minibuffer-map ("M-RET" . ivy-immediate-done))
  (:map counsel-find-file-map ("C-~" . counsel-goto-local-home)))
 :custom
 (ivy-use-virtual-buffers t)
 (ivy-height 10)
 (ivy-on-del-error-function nil)
 (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
 (ivy-count-format " %d/%d  ")
 (ivy-wrap t)
 :config
 (defun counsel-goto-local-home ()
   "Go to the $HOME of the local machine."
   (interactive)
   (ivy--cd "~/")))

;; ============================================================================
;; color ripgrep
(use-package
 color-rg
 :load-path
 (lambda ()
   (expand-file-name "site-elisp/color-rg" user-emacs-directory))
 :if (executable-find "rg")
 :bind ("C-M-s" . color-rg-search-input))

;; ============================================================================
;; Winner, a mode to restore previous window layouts
(use-package
 winner
 :ensure nil
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

;; ============================================================================
;; Which Key, a feature that displays the key bindings following the incomplete command.
(use-package
 which-key
 :diminish
 :custom
 (which-key-separator " ")
 (which-key-prefix-prefix "+")
 :config (which-key-mode))

;; ============================================================================
;; Undo tree, a feature that provides a visualization of the undos in a file.
(use-package
 undo-tree
 :defer t
 :diminish undo-tree-mode
 :init (global-undo-tree-mode)
 :custom (undo-tree-visualizer-diff t)
 (undo-tree-history-directory-alist
  `(("." . ,(expand-file-name ".backup" user-emacs-directory))))
 (undo-tree-visualizer-timestamps t))

;; ============================================================================
;; Ace Window, a package for selecting windows to switch to.
(use-package ace-window)

;; ============================================================================
;; History.
(use-package
 recentf
 :ensure nil
 :hook (after-init . recentf-mode)
 :custom
 (recentf-auto-cleanup "05:00am")
 (recentf-max-saved-items 200)
 (recentf-exclude
  '((expand-file-name package-user-dir)
    ".cache"
    ".cask"
    ".elfeed"
    "bookmarks"
    "cache"
    "ido.*"
    "persp-confs"
    "recentf"
    "undo-tree-hist"
    "url"
    "COMMIT_EDITMSG\\'")))

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)

;; Move the backup fies to user-emacs-directory/.backup
(setq backup-directory-alist
      `(("." . ,(expand-file-name ".backup" user-emacs-directory))))

;; Ask before killing emacs
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Automatically kill all active processes when closing Emacs
(setq confirm-kill-processes nil)

;; Turn Off Cursor Alarms
(setq ring-bell-function 'ignore)

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;; Don't Lock Files
(setq-default create-lockfiles nil)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)

;; ad-handle-definition warnings are generated when functions are redefined with `defadvice',
;; they are not helpful.
(setq ad-redefinition-action 'accept)

;; Move Custom-Set-Variables to Different File
(setq custom-file
      (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Enable `erase-buffer' function
(put 'erase-buffer 'disabled nil)

(provide 'init-core)
;;; init-core.el ends here