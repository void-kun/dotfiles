;;; init-custom.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)

(setq user-full-name "zrik")
(setq user-mail-address "lolizilker@gmail.com")

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

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

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)
;;; init-custom.el ends here
