;;; lolo-prog.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 treesit-auto
 :custom (treesit-auto-install 'prompt))

(with-eval-after-load 'treesit
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (go-mode . go-ts-mode)
          (c-mode . c-ts-mode))))

;; Code styles
(use-package
 editorconfig
 :diminish
 :hook (after-init . editorconfig-mode))

;; Browse devdocs.io documents using EWW
(use-package
 devdocs
 :autoload (devdocs--installed-docs devdocs--available-docs)
 :bind (:map prog-mode-map ("C-h D" . devdocs-dwim))
 :init
 (defconst devdocs-major-mode-docs-alist
   '((c-mode . ("c"))
     (c++-mode . ("cpp"))
     (python-mode . ("python~3.11" "python~2.7"))
     (rustic-mode . ("rust"))
     (css-mode . ("css"))
     (html-mode . ("html"))
     (js-mode . ("javascript" "jquery"))
     (js2-mode . ("javascript" "jquery"))
     (emacs-lisp-mode . ("elisp")))
   "Alist of major-mode and docs.")

 (mapc
  (lambda (mode)
    (add-hook
     (intern (format "%s-hook" (car mode)))
     (lambda () (setq-local devdocs-current-docs (cdr mode)))))
  devdocs-major-mode-docs-alist)

 (setq devdocs-data-dir
       (expand-file-name "devdocs" user-emacs-directory))

 (defun devdocs-dwim ()
   "Look up a DevDocs documentation entry.
Install the doc if it's not installed."
   (interactive)
   ;; Install the doc if it's not installed
   (mapc
    (lambda (slug)
      (unless (member
               slug
               (let ((default-directory devdocs-data-dir))
                 (seq-filter
                  #'file-directory-p
                  (when (file-directory-p devdocs-data-dir)
                    (directory-files "." nil "^[^.]")))))
        (mapc
         (lambda (doc)
           (when (string= (alist-get 'slug doc) slug)
             (devdocs-install doc)))
         (devdocs--available-docs))))
    (alist-get major-mode devdocs-major-mode-docs-alist))

   ;; Lookup the symbol at point
   (devdocs-lookup nil (thing-at-point 'symbol t))))

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; formatter
(use-package
 format-all
 :commands format-all-mode
 :hook (prog-mode . format-all-mode)
 :config
 (setq-default format-all-formatters
               '(("C" (clang-format "-style=file"))
                 ("C++" (clang-format "-style=file"))
                 ("Go" (gofmt))
                 ("Rust" (rustfmt))
                 ("Shell" (shfmt "-i" "4" "-ci"))
                 ("TypeScript" (prettier))
                 ("TSX" (prettier))
                 ("JavaScript" (prettier))
                 ("JSX" (prettier))
                 ("Json" (prettier))
                 ("Vue" (prettier))
                 ("Python" (black))
                 ("CMake" (cmake-format))
                 ("CSS" (prettier))
                 ("SCSS" (prettier))
                 ("Less" (prettier)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Eglot

(use-package
 eglot
 :commands eglot
 :init (setq eglot-stay-out-of '(flycheck))
 :custom
 (eglot-ignored-server-capabilites '(:documentHighlightProvider))
 (eglot-autoshutdown t)
 :hook
 (eglot-managed-mode . eldoc-box-hover-mode)
 (c++-mode . eglot-ensure)
 (c-ts-mode . eglot-ensure)
 (go-ts-mode . eglot-ensure)
 (rustic-mode . eglot-ensure)
 (javascript-ts-mode . eglot-ensure))

(with-eval-after-load 'treesit
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (go-mode . go-ts-mode)
          (c-mode . c-ts-mode))))

(with-eval-after-load 'eglot
  (load-library "project"))

(use-package
 eldoc-box
 :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
 :custom (eldoc-box-clear-with-C-g t))

(provide 'lolo-prog)
;;; lolo-prog.el ends here
