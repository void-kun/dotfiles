;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; Custom variables.
(setq user-full-name "zrik")
(setq user-mail-address "lolizilker@gmail.com")

(defcustom lolo nil
  "Lolo emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/void-kun"))

(defcustom lolo-var-proxy "127.0.0.1:1087"
  "Set HTTP/HTTPS proxy"
  :group 'lolo
  :type 'string)

(defcustom lolo-var-socks-proxy "127.0.0.1:1086"
  "Set SOCKS proxy"
  :group 'lolo
  :type 'string)

(defcustom lolo-var-server t
  "Enable server."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-var-auto-save t
  "Enable autosave."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-var-format-on-save t
  "Enable format on save."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-var-whitespace t
  "Enable whitespace."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-var-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `lolo-var-whitespace' is also enabled."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-var-flyspell t
  "Enable flyspell support."
  :group 'lolo
  :type 'boolean)

(defcustom lolo-var-theme 'modus
  "The default color theme."
  :group 'lolo
  :type
  '(choice
    :tag "Set of themes to load"
    :value
    (const :tag "The `gruber-darker-theme' module" gruber-darker)
    (const :tag "The `ef-theme' module" ef)
    (const :tag "The `modus-theme' module" modus)
    (const :tag "The `standard-theme' module" standard)
    (const :tag "Do not load a theme module" nil)))

(defcustom lolo-var-theme-dark nil
  "The default theme style."
  :group 'lolo
  :type 'boolean)

;; ============================================================================
;; Define variables.
(defvar lolo/default-font-size 120
  "Default font size.")

(defvar lolo/default-font-name "IosevkaLyteTerm"
  "Default font.")

(provide 'lolo-customs)
;;; lolo-customs.el ends here
