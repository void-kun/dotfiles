;;; lolo-web.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; CSS
(use-package css-mode :init (setq css-indent-offset 2))

;; SCSS
(use-package scss-mode :init (setq scss-compile-at-save nil))

;; LESS
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; JSON
(unless (fboundp 'js-json-mode)
  (use-package json-mode))

;; JavaScript
(use-package js :init (setq js-indent-level 2))

(use-package
 js2-mode
 :mode (("\\.js\\'" . js2-mode) ("\\.jsx\\'" . js2-jsx-mode))
 :interpreter (("node" . js2-mode) ("node" . js2-jsx-mode))
 :hook
 ((js2-mode . js2-imenu-extras-mode)
  (js2-mode . js2-highlight-unused-variables-mode))
 :config
 ;; Use default keybindings for lsp
 (unbind-key "M-." js2-mode-map))

;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
(when (executable-find "prettier")
  (use-package
   prettier
   :diminish
   :hook
   ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
   :init (setq prettier-pre-warm 'none)))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package
 skewer-mode
 :diminish
 :functions diminish
 :hook
 (((js-mode js2-mode) . skewer-mode)
  (css-mode . skewer-css-mode)
  ((html-mode web-mode) . skewer-html-mode))
 :init
 ;; diminish
 (with-eval-after-load 'skewer-css
   (diminish 'skewer-css-mode))
 (with-eval-after-load 'skewer-html
   (diminish 'skewer-html-mode)))

(use-package typescript-mode :mode ("\\.ts[x]\\'" . typescript-mode))

;; Run Mocha or Jasmine tests
(use-package mocha :config (use-package mocha-snippets))

;; Major mode for editing web templates
(use-package
 web-mode
 :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
 :config
 (setq web-mode-markup-indent-offset 2)
 (setq web-mode-css-indent-offset 2)
 (setq web-mode-code-indent-offset 2))

;; Adds node_modules/.bin directory to `exec_path'
(use-package
 add-node-modules-path
 :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(use-package haml-mode)

;; REST
(use-package
 restclient
 :mode ("\\.http\\'" . restclient-mode)
 :config
 (use-package
  restclient-test
  :diminish
  :hook (restclient-mode . restclient-test-mode)))

;; web-mode setup
(define-derived-mode
 pbgc-vue-mode
 web-mode
 "pbVue"
 "A major mode derived from web-mode, for editing .vue files with LSP support.")


(add-to-list 'auto-mode-alist '("\\.vue\\'" . pbgc-vue-mode))

(defun vue-custom ()
  (flycheck-mode t)
  (eglot-ensure))
(add-hook 'vue-mode-hook 'vue-custom)

(defun vue-eglot-init-options ()
  (let ((tsdk-path
         (expand-file-name
          "lib"
          (string-trim-right
           (shell-command-to-string
            "pnpm list --global --parseable typescript | head -n1")))))
    `(:typescript
      (:tsdk "/home/zrik/.local/share/pnpm/global/5/node_modules/typescript/lib"
       :languageFeatures
       (:completion
        (:defaultTagNameCase
         "both"
         :defaultAttrNameCase "kebabCase"
         :getDocumentNameCasesRequest nil
         :getDocumentSelectionRequest nil)
        :diagnostics (:getDocumentVersionRequest nil))
       :documentFeatures
       (:documentFormatting
        (:defaultPrintWidth 100 :getDocumentPrintWidthRequest nil)
        :documentSymbol t
        :documentColor t)))))

(with-eval-after-load 'eglot
    ;; Volar
    (add-to-list
    'eglot-server-programs
    `(pbgc-vue-mode
    .
    ("vue-language-server"
        "--stdio"
        :initializationOptions ,(vue-eglot-init-options)))))

(provide 'lolo-web)
;;; lolo-web.el ends here
