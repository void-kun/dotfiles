;;; init-web.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 web-mode
 :custom-face
 (css-selector ((t (:inherit default :foreground "#66CCFF"))))
 (font-lock-comment-face ((t (:foreground "#828282"))))
 :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
 :config
 (setq web-mode-markup-indent-offset 2)
 (setq web-mode-css-indent-offset 2)
 (setq web-mode-code-indent-offset 2))

;; Adds node_modules/.bin directory to `exec_path'
(use-package add-node-modules-path
  :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(use-package js
  :init (setq js-indent-level 2))

(use-package
 js2-mode
 :mode (("\\.js\\'" . js2-mode)
	("\\.jsx\\'" . js2-jsx-mode))
 :interpreter (("node" . js2-mode)
	       ("node" . js2-jsx-mode))
 :hook ((js2-mode . js2-imenu-extras-mode)
	(js2-mode . js2-highlight-unused-variables-mode))
 :bind (:map js-mode-map ("M-." . nil)))

(use-package
 typescript-mode
 :mode "\\.ts[x]\\'"
 :commands (typescript-mode))

(use-package prettier
  :diminish
  :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
  :init (setq prettier-pre-warn 'none))

;;; Live browser Javascript, css, and HTML interaction
(use-package skewer-mode
  :diminish
  :functions diminish
  :hook (((js-mode js2-mode)   . skewer-mode)
	 (css-mode             . skewer-mode)
	 ((html-mode web-mode) . skewer-mode))
  :init
  ;; diminish
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))

(use-package
 emmet-mode
 :hook ((web-mode . emmet-mode) (css-mode . emmet-mode)))

(use-package
 instant-rename-tag
 :load-path
 (lambda ()
   (expand-file-name "site-elisp/instant-rename-tag"
                     user-emacs-directory))
 :bind ("C-z <" . instant-rename-tag))

(use-package json-mode :mode "\\.json\\'")

;; REST
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode)))

(provide 'init-web)
;;; init-web.el ends here
