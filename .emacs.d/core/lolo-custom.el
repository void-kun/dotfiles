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
  :group 'convenience)

(defcustom org-directory (expand-file-name "~/org")
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

;; File/directory locations --------
(defvar lolo/home (concat (getenv "HOME") "/")
  "My home directory.")
(defvar lolo/dropbox (concat lolo/home "code/fun/")
  "Dropbox directory.")
(defvar lolo/emacs-stuff (concat lolo/home "/.emacs.d/stuff")
  "Emacs stuff folder.")

;; Internal use variables
(defvar lolo-text-height nil
  "My preferred default text height.")
(defvar lolo-doom-modeline-text-height
  nil "My preferred modeline text height.")
(defvar lolo-default-line-spacing 0
  "Baseline line spacing.")
(setq-default lolo-default-line-spacing 0)

;; Emacs variables
(setq bookmark-default-file (concat lolo/emacs-stuff "/bookmarks"))
(setq custom-theme-directory (expand-file-name "themes" lolo/emacs-stuff))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'lolo-custom)
;;; lolo-custom.el ends here
