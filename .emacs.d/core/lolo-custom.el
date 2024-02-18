;;; lolo-custom.el --------------------------------

;; customize
(defgroup lolo nil
    "Emacs lolo configuration"
    :prefix "lolo-"
    :group 'convenience)

(defcustom lolo-minimalistic-ui nil
    "Controls display style"
    :type 'boolean
    :group 'lolo
    :package-version '(lolo . "0.1"))

(defcustom lolo-super-keybindings t
    "Enable super key bindings"
    :type 'boolean
    :group 'lolo
    :package-version '(lolo . "0.1"))

(defcustom lolo-auto-save t
    "Auto save"
    :type 'boolean
    :group 'lolo)

(defcustom lolo-clean-whitespace-on-save t
    "Cleanup whitespace from file before it's saved."
    :type 'boolean
    :group 'lolo)

(defcustom lolo-indent-sensitive-modes
    '(conf-mode python-mode yaml-mode rust-mode c-mode)
    "Modes for which auto-indenting is suppressed."
    :type 'list
    :group 'lolo)

(defcustom lolo-format-on-save t
    "Run mode specific format on file before it's saved."
    :type 'boolean
    :group 'lolo)

(defcustom lolo-whitespace t
  "Non-nil values enable whitespace"
  :type 'boolean
  :group 'lolo)

(defcustom lolo-flyspell t
  "Flyspell enable"
  :type 'boolean
  :group 'lolo)

(defcustom lolo-guru nil
  "Guru"
  :type 'boolean
  :group 'lolo)

;; themes:
;; + gruber-darker
;; + monokai-pro
(defcustom lolo-theme 'gruber-darker
    "The default color theme."
    :type'string
    :group 'lolo)

(provide 'lolo-custom)
;;; lolo-custom.el ends here
