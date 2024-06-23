;;; init-packages.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
    (require 'init-custom)
    (require 'cl-lib)
    (require 'package))

;;; Package setups
(add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") t)

;; Load the pinned packages
(let ((lolo-pinned-packages-file (expand-file-name "pinned-packages.el" lolo-dir)))
    (if (file-exists-p lolo-pinned-packages-file)
        (load lolo-pinned-packages-file)))

(setq lolo-user-dir (expand-file-name "elpa" lolo-dir))

;; install & enable use-package
(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(defvar lolo-packages
    '(ace-window
      avy
      anzu
      browse-kill-ring
      crux
      diff-hl
      diminish
      easy-kill
      editorconfig
      epl
      expand-region
      flycheck
      gist
      git-timemachine
      git-modes
      guru-mode
      hl-todo
      imenu-anywhere
      projectile
      magit
      nlinum
      operate-on-number
      smartrep
      super-save
      undo-tree
      volatile-highlights
      which-key
      gruber-darker-theme
      zop-to-char)
      "List of packages to ensure are installed at launch.")

(defun lolo/packages-installed-p ()
  "Check if all packages in `lolo-packages' are installed."
  (cl-every #'package-installed-p lolo-packages))

(defun lolo/require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package lolo-packages)
    (add-to-list 'lolo-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun lolo/require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'lolo/require-package packages))

(defun lolo/install-packages ()
  "Install all packages listed in `lolo-packages'."
  (unless (lolo/packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs lolo is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (lolo/require-packages lolo-packages)))

;; run package installation
(lolo/install-packages)

(defun lolo/list-foreign-packages ()
  "Browse third-party packages not bundled with lolo.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `lolo-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (cl-set-difference package-activated-list lolo-packages)))

(defmacro lolo-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

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

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (lolo-auto-install extension package mode))))
 lolo-auto-install-alist)

(use-package smartparens
  :load-path "../site-lisp/smartparens"
  :defer t
  :ensure t)

(provide 'init-packages)
;;; init-packages.el ends here
