;;; init-custom.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; The global variable about Lolo Emacs.
;;
;;; Code:

(defcustom lolo nil
    "Lolo Emacs customization."
    :group 'convenience
    :link '(url-link :tag "Homepage" "https://github.com/void-kun/dotfiles"))

(defcustom lolo-proxy "127.0.0.1:1087"
    "Set HTTP/HTTPS proxy"
    :group 'lolo
    :type 'string)

(defcustom lolo-socks-proxy "127.0.0.1:1086"
    "Set SOCKS proxy"
    :group 'lolo
    :type 'string)

(defcustom lolo-server t
    "Enable server"
    :group 'lolo
    :type 'boolean)

(defcustom lolo-auto-save t
    "Enable autosave"
    :group 'lolo
    :type 'boolean)

(defcustom lolo-format-on-save t
    "Enable format on save"
    :group 'lolo
    :type 'boolean)

(defcustom lolo-whitespace t
    "Enable whitespace"
    :group 'lolo
    :type 'boolean)

(defcustom lolo-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `lolo-whitespace' is also enabled."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-flyspell t
    "Enable flyspell support"
    :group 'lolo
    :type 'boolean)

(defcustom lolo-super-keybindings t
    "Enable flyspell support"
    :group 'lolo
    :type 'boolean)

(defcustom lolo-yank-indent-modes '(LaTeX-mode TeX-mode)
    "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
    :group 'lolo
    :type 'list)

(defcustom lolo-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list
  :group 'lolo)

(defcustom lolo-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'lolo)


(defcustom lolo-minimalistic-ui nil
    "Controls whether to display the menu-bar and line numbers.
Note that the toolbar is always hidden regardless of this settings."
    :group 'lolo
    :type 'boolean)

(defcustom lolo-theme 'gruber-darker
    "The default color theme"
    :group 'lolo
    :type 'symbol)

(provide 'init-custom)
;;; init-custom.el ends here
