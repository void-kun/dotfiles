
;;; lolo-prog.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'lolo-const)
  (require 'lolo-custom))

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
    :ensure nil
    :hook (prog-mode . prettify-symbols-mode)
    :init
    (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Tree-sitter support
(when (lolo-treesit-available-p)
  (use-package treesit-auto
      :hook (after-init . global-treesit-auto-mode)
      :init (setq treesit-auto-install 'prompt)))

;; Show function arglist or variable docstring
(use-package eldoc
    :ensure nil
    :diminish
    :config
    (when (childframe-workable-p)
      (use-package eldoc-box
          :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
          :custom
          (eldoc-box-lighter nil)
          (eldoc-box-only-multi-line t)
          (eldoc-box-clear-with-C-g t)
          :custom-face
          (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
          (eldoc-box-body ((t (:inherit tooltip))))
          :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
          :config
          ;; Prettify `eldoc-box' frame
          (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
                (alist-get 'right-fringe eldoc-box-frame-parameters) 8))))

;; Search tool
(use-package grep
    :ensure nil
    :autoload grep-apply-setting
    :init
    (when (executable-find "rg")
      (grep-apply-setting
       'grep-command "rg --color=auto --null -nH --no-heading -e ")
      (grep-apply-setting
       'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
      (grep-apply-setting
       'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
      (grep-apply-setting
       'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))

;; Cross-referencing commands
(use-package xref
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;; Jump to definition
(use-package dumb-jump
  :pretty-hydra
  ((:title (pretty-hydra-title "Dump Jump" 'faicon "nf-fa-anchor")
           :color blue :quit-key ("q" "C-g"))
   ("Jump"
    (("j" dumb-jump-go "Go")
     ("o" dumb-jump-go-other-window "Go other window")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
    "Other"
    (("i" dumb-jump-go-prompt "Prompt")
     ("l" dumb-jump-quick-look "Quick look")
     ("b" dumb-jump-back "Back"))))
  :bind (("C-M-j" . dumb-jump-hydra/body))
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read))

;; Code styles
(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; Run commands quickly
(use-package quickrun
    :bind (("C-<f5>" . quickrun)
           ("C-c X"  . quickrun)))

;; ;; Browse devdocs.io documents using EWW
;; (use-package devdocs
;;   :autoload (devdocs--installed-docs devdocs--available-docs)
;;   :bind (:map prog-mode-map
;;          ("M-<f1>" . devdocs-dwim)
;;          ("C-h D"  . devdocs-dwim))
;;   :init
;;   (defconst devdocs-major-mode-docs-alist
;;     '((c-mode          . ("c"))
;;       (c++-mode        . ("cpp"))
;;       (python-mode     . ("python~3.12" "python~2.7"))
;;       (rustic-mode     . ("rust"))
;;       (css-mode        . ("css"))
;;       (html-mode       . ("html"))
;;       (js-mode         . ("javascript" "jquery"))
;;       (js2-mode        . ("javascript" "jquery"))
;;       (emacs-lisp-mode . ("elisp")))
;;     "Alist of major-mode and docs.")

;;   (mapc
;;    (lambda (mode)
;;      (add-hook (intern (format "%s-hook" (car mode)))
;;                (lambda ()
;;                  (setq-local devdocs-current-docs (cdr mode)))))
;;    devdocs-major-mode-docs-alist)

;;   (setq devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))

;;   (defun devdocs-dwim()
;;     "Look up a DevDocs documentation entry.

;; Install the doc if it's not installed."
;;     (interactive)
;;     ;; Install the doc if it's not installed
;;     (mapc
;;      (lambda (slug)
;;        (unless (member slug (let ((default-directory devdocs-data-dir))
;;                               (seq-filter #'file-directory-p
;;                                           (when (file-directory-p devdocs-data-dir)
;;                                             (directory-files "." nil "^[^.]")))))
;;          (mapc
;;           (lambda (doc)
;;             (when (string= (alist-get 'slug doc) slug)
;;               (devdocs-install doc)))
;;           (devdocs--available-docs))))
;;      (alist-get major-mode devdocs-major-mode-docs-alist))

;;     ;; Lookup the symbol at point
;;     (devdocs-lookup nil (thing-at-point 'symbol t))))

;; ;; Misc. programming modes
(use-package csv-mode)
(use-package cask-mode)
(use-package cmake-mode)
(use-package mermaid-mode)
(use-package yaml-mode)

(use-package protobuf-mode
  :hook (protobuf-mode . (lambda ()
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

(use-package nxml-mode
    :ensure nil
    :mode (("\\.xaml$" . xml-mode)))

(provide 'lolo-prog)
;; ;;; lolo-prog.el ends here
