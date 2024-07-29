;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 emmet-mode
 :straight (:build t)
 :defer t
 :hook
 ((css-mode . emmet-mode)
  (html-mode . emmet-mode)
  (web-mode . emmet-mode)
  (sass-mode . emmet-mode)
  (scss-mode . emmet-mode)
  (web-mode . emmet-mode)))

(use-package impatient-mode :straight (:build t) :defer t)

(use-package
 web-mode
 :defer t
 :straight (:build t)
 :hook html-mode
 :hook (web-mode . prettier-js-mode)
 :hook (web-mode . lsp-deferred)
 :mode
 (("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.twig\\'" . web-mode)
  ("\\.xml\\'" . web-mode)
  ("\\.html\\'" . web-mode)
  ("\\.htm\\'" . web-mode)
  ("\\.[gj]sp\\'" . web-mode)
  ("\\.as[cp]x?\\'" . web-mode)
  ("\\.eex\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.handlebars\\'" . web-mode)
  ("\\.hbs\\'" . web-mode)
  ("\\.eco\\'" . web-mode)
  ("\\.ejs\\'" . web-mode)
  ("\\.svelte\\'" . web-mode)
  ("\\.ctp\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.vue\\'" . web-mode))
 :config
 (csetq
  web-mode-markup-indent-offset
  2
  web-mode-code-indent-offset
  2
  web-mode-css-indent-offset
  2
  web-mode-style-padding
  0
  web-mode-script-padding
  0))

(use-package
 company-web
 :defer t
 :straight (:build t)
 :after (emmet-mode web-mode))

;; ============================================================================
(use-package astro-ts-mode :straight (:build t) :defer t)

;; ============================================================================
(use-package
 css-mode
 :defer t
 :straight (:type built-in)
 :hook (css-mode . smartparens-mode)
 :hook (css-mode . lsp-deferred)
 :hook (scss-mode . prettier-js-mode)
 :init (put 'css-indent-offset 'safe-local-variable #'integerp))


(use-package
 scss-mode
 :straight (:build t)
 :hook (scss-mode . smartparens-mode)
 :hook (scss-mode . lsp-deferred)
 :hook (scss-mode . prettier-js-mode)
 :defer t
 :mode "\\.scss\\'")

(use-package
 counsel-css
 :straight (:build t)
 :defer t
 :init
 (cl-loop
  for
  (mode-map . mode-hook)
  in
  '((css-mode-map . css-mode-hook) (scss-mode-map . scss-mode-hook))
  do
  (add-hook mode-hook #'counsel-css-imenu-setup)))

(use-package
 less-css-mode
 :straight (:type built-in)
 :defer t
 :mode "\\.less\\'"
 :hook (less-css-mode . smartparens-mode)
 :hook (less-css-mode . lsp-deferred)
 :hook (less-css-mode . prettier-js-mode))


;; ============================================================================
(use-package
 rjsx-mode
 :defer t
 :straight (:build t)
 :after compile
 :mode "\\.[mc]?jsx?\\'"
 :mode "\\.es6\\'"
 :mode "\\.pac\\'"
 :interpreter "node"
 :hook (rjsx-mode . rainbow-delimiters-mode)
 :hook (rjsx-mode . lsp-deferred)
 :init (add-to-list 'compilation-error-regexp-alist 'node)
 (add-to-list
  'compilation-error-regexp-alist-alist
  '(node
    "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
    2
    3
    4))
 :config
 (setq
  js-chain-indent t
  js2-basic-offset 2
  ;; ignore shebangs
  js2-skip-preprocessor-directives t
  ;; Flycheck handles this already
  js2-mode-show-parse-errors nil
  js2-mode-show-strict-warnings nil
  ;; conflicting with eslint, Flycheck already handles this
  js2-strict-missing-semi-warning nil
  js2-highlight-level 3
  js2-idle-timer-delay 0.15))

(use-package
 js2-refactor
 :defer t
 :straight (:build t)
 :after (js2-mode rjsx-mode)
 :hook (js2-mode . js2-refactor-mode)
 :hook (rjsx-mode . js2-refactor-mode))

(use-package
 prettier-js
 :defer t
 :straight (:build t)
 :after (rjsx-mode web-mode typescript-mode)
 :hook ((rjsx-mode typescript-mode) . prettier-js-mode)
 :config (setq prettier-js-args '("--single-quote" "--jsx-single-quote")))

;; ============================================================================
(use-package
 typescript-mode
 :defer t
 :straight (:build t)
 :hook (typescript-mode . rainbow-delimiters-mode)
 :hook (typescript-tsx-mode . rainbow-delimiters-mode)
 :hook (typescript-mode . lsp-deferred)
 :hook (typescript-tsx-mode . lsp-deferred)
 :hook (typescript-mode . prettier-js-mode)
 :hook (typescript-tsx-mode . prettier-js-mode)
 :commands typescript-tsx-mode
 :after flycheck
 :init (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
 :config
 (with-eval-after-load 'flycheck
   (flycheck-add-mode 'javascript-eslint 'web-mode)
   (flycheck-add-mode 'javascript-eslint 'typescript-mode)
   (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
   (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode))
 (when (fboundp 'web-mode)
   (define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX"))
 (autoload 'js2-line-break "js2-mode" nil t))

(use-package
 tide
 :defer t
 :straight (:build t)
 :hook (tide-mode . tide-hl-identifier-mode)
 :config
 (setq
  tide-completion-detailed t
  tide-always-show-documentation t
  tide-server-may-response-length 524288
  tide-completion-setup-company-backend nil)

 (advice-add #'tide-setup :after #'eldoc-mode))

(provide 'lolo-web)
;;; lolo-web.el ends here
