;;; lolo-packages.el --------------------------------
(require 'cl-lib)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; load the pinned packages
(let ((lolo-pinned-packages-file (expand-file-name "lolo-pinned-packages.el" lolo-core-dir)))
    (if (file-exists-p lolo-pinned-packages-file)
        (load lolo-pinned-packages-file)))

;; set package-user-dir to be relative to Lolo install path
(setq package-user-dir (expand-file-name "elpa" lolo-dir))
(package-initialize)

;; install and enable use-package
(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(defvar lolo-packages
    '(ace-window
      ag
      avy
      anzu
      browse-kill-ring
      crux
      discover-my-major
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
      move-text
      nlinum
      operate-on-number
      smartparens
      smartrep
      super-save
      undo-tree
      volatile-highlights
      which-key
      monokai-pro-theme
      zop-to-char)
    "A list of packages to ensure are installed at launch.")

(defun lolo-packages-installed-p ()
    "Check if all packages in `lolo-package' are installed."
    (cl-every #'package-installed-p lolo-packages))

(defun lolo-require-package (package)
    "Install package unless already installed."
    (unless (memq package lolo-packages)
        (add-to-list 'lolo-packages package))
    (unless (package-installed-p package)
        (package-install package)))

(defun lolo-require-packages (packages)
    "Ensure packages are installed."
    (mapc #'lolo-require-package packages))

(defun lolo-install-packages ()
    "Install all packages listed in `lolo-packages'."
    (unless (lolo-packages-installed-p)
        (message "%s" "Emacs Lolo is now refreshing its package database...")
        (package-refresh-contents)
        (message "%s" " done.")
        (lolo-require-packages lolo-packages)))

;; run package installation
(lolo-install-packages)

(defun lolo-list-foreign-packages ()
    "Browse third-party packages not bundled with Lolo."
    (interactive)
    (package-show-package-list
        (cl-set-difference package-activated-list lolo-packages)))

;; auto install of major modes on demand
(defmacro lolo-auto-install (extension package mode)
    "Auto install"
    `(add-to-list 'auto-mode-alist
                  `(,extension . (lambda ()
                                    (unless (package-installed-p ',package)
                                        (package-install ',package))
                                    (,mode)))))

(defvar lolo-auto-install-alist
    '(("\\.cmake\\'" cmake-mode cmake-mode)
      ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
      ("\\.css\\'" css-mode css-mode)
      ("\\.csv\\'" csv-mode csv-mode)
      ("\\.feature\\'" feature-mode feature-mode)
      ("\\.go\\'" go-mode go-mode)
      ("\\.graphql\\'" graphql-mode graphql-mode)
      ("\\.json\\'" json-mode json-mode)
      ("\\.kv\\'" kivy-mode kivy-mode)
      ("\\.latex\\'" auctex LaTeX-mode)
      ("\\.lua\\'" lua-mode lua-mode)
      ("\\.markdown\\'" markdown-mode markdown-mode)
      ("\\.md\\'" markdown-mode markdown-mode)
      ("\\.proto\\'" protobuf-mode protobuf-mode)
      ("\\.pyd\\'" cython-mode cython-mode)
      ("\\.pyi\\'" cython-mode cython-mode)
      ("\\.pyx\\'" cython-mode cython-mode)
      ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
      ("\\.rs\\'" rust-mode rust-mode)
      ("\\.sass\\'" sass-mode sass-mode)
      ("\\.scala\\'" scala-mode scala-mode)
      ("\\.scss\\'" scss-mode scss-mode)
      ("\\.styl\\'" stylus-mode stylus-mode)
      ("\\.swift\\'" swift-mode swift-mode)
      ("\\.textile\\'" textile-mode textile-mode)
      ("\\.yml\\'" yaml-mode yaml-mode)
      ("\\.yaml\\'" yaml-mode yaml-mode)
      ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

(when (package-installed-p 'markdown-mode)
    (add-to-list 'auto-mode-alist '("\\.mardown\\'" . gfm-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

(when (package-installed-p 'pkgbuild-mode)
    (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

(mapc (lambda (entry)
            (let ((extension (car entry))
                  (package (cadr entry))
                  (mode (caddr (cdr entry))))
            (unless (package-installed-p package)
                (lolo-auto-install extension package mode))))
      lolo-auto-install-alist)

(provide 'lolo-packages)
;;; lolo-packages.el ends here
