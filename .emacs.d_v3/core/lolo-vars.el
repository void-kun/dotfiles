;;; lolo-vars.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; Unbind unneeded keys.
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
(global-set-key (kbd "C-d") nil)
(global-set-key (kbd "M-<backspace>") nil)

;; ============================================================================
;; Custom variables.
(setq user-full-name "zrik")
(setq user-mail-address "lolizilker@gmail.com")

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
  "Enable server."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-auto-save t
  "Enable autosave."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-format-on-save t
  "Enable format on save."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-whitespace t
  "Enable whitespace."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `lolo-whitespace' is also enabled."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-flyspell t
  "Enable flyspell support."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-theme 'gruber-darker
  "The default color theme."
  :group 'lolo
  :type
  '(choice
    :tag "Set of themes to load"
    :value
    modus
    (const :tag "The `gruber-darker-theme' module" gruber-darker)
    (const :tag "The `ef-theme' module" ef)
    (const :tag "The `modus-theme' module" modus)
    (const :tag "The `standard-theme' module" standard)
    (const :tag "Do not load a theme module" nil)))

(defcustom lolo-theme-dark t
  "The default theme style."
  :group 'lolo
  :type 'boolean)

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" lolo-dir))

(provide 'lolo-vars)
;;; lolo-vars.el ends here
