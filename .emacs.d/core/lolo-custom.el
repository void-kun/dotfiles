
;;; lolo-custom.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(eval-when-compile
  (require 'package))

(defgroup lolo nil
  "Lolo Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/void-kun/dotfiles"))

(defcustom lolo-org-directory (expand-file-name "~/org")
  "Set org directory."
  :group 'lolo
  :type 'string)

(defcustom lolo-proxy "127.0.0.1:1087"
  "Set HTTP/HTTPS proxy."
  :group 'lolo
  :type 'string)

(defcustom lolo-socks-proxy "127.0.0.1:1086"
  "Set SOCKS proxy."
  :group 'lolo
  :type 'string)

(defcustom lolo-server t
  "Enable `server-mode' or not."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-icon t
  "Display icons or not."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-package-archives-alist
  (let ((proto (if (gnutls-available-p) "https" "http")))
    `((melpa    . (("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
                   ("melpa"  . ,(format "%s://melpa.org/packages/" proto))))))
  "A list of the package archives."
  :group 'lolo
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom lolo-package-archives 'melpa
  "Set package archives from which to fetch."
  :group 'lolo
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value lolo-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    lolo-package-archives-alist)))

(defcustom lolo-completion-style 'childframe
  "Completion display style."
  :group 'lolo
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom lolo-dashboard (not (daemonp))
  "Display dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-lsp 'lsp-mode
  "Set language server.

`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
nil means disabled."
  :group 'lolo
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

(defcustom lolo-tree-sitter t
  "Enable tree-sitter or not.
Native tree-sitter is introduced in 29."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-lsp-format-on-save nil
  "Auto format buffers on save."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes."
  :group 'lolo
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom lolo-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "A list of symbol prettifications.
Nil to use font supports ligatures."
  :group 'lolo
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'lolo-custom)
;;; lolo-custom.el ends here
