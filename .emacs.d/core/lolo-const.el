;;; lolo-const.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ================================ const ================================

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")
  
(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(eval-when-compile
  (require 'package))

;; ================================ custom ================================

;; customize
(defgroup lolo nil
  "Emacs lolo configuration."
  :prefix "lolo-"
  :group 'convenience)

(defcustom lolo-minimalistic-ui nil
  "Controls whether to display the menu-bar and line numbers.
Note that the toolbar is always hidden regardless of this setting."
  :type 'boolean
  :group 'lolo)

(defcustom lolo-super-keybindings t
  "Controls whether to use the Super key in keybindings.
They can be problematic in some operating systems (e.g. Windows)
or desktop environments that make heavy use of them."
  :type 'boolean
  :group 'lolo)

(defcustom lolo-auto-save t
  "Non-nil values enable lolo's auto save."
  :type 'boolean
  :group 'lolo)

(defcustom lolo-guru t
  "Non-nil values enable `guru-mode'."
  :type 'boolean
  :group 'lolo)

(defcustom lolo-whitespace t
  "Non-nil values enable lolo's whitespace visualization."
  :type 'boolean
  :group 'lolo)

(defcustom lolo-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `lolo-whitespace' is also enabled."
  :type 'boolean
  :group 'lolo)

(defcustom lolo-flyspell t
  "Non-nil values enable lolo's flyspell support."
  :type 'boolean
  :group 'lolo)

(defcustom lolo-user-init-file (expand-file-name "personal/"
                                                    user-emacs-directory)
  "Path to your personal customization file.
lolo recommends you only put personal customizations in the
personal folder.  This variable allows you to specify a specific
folder as the one that should be visited when running
`crux-find-user-init-file'.  This can be easily set to the desired buffer
in Lisp by putting `(setq lolo-user-init-file load-file-name)'
in the desired elisp file."
  :type 'string
  :group 'lolo)

(defcustom lolo-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list
  :group 'lolo)

(defcustom lolo-format-on-save t
  "Run mode specific format on file before it's saved.
Currently only applies to tide-mode."
  :type 'boolean
  :group 'lolo)

(defcustom lolo-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'lolo)

(defcustom lolo-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'lolo)

(defcustom lolo-theme 'gruber-darker
  "The default color theme, change this in your /personal/preload config."
  :type 'symbol
  :group 'lolo)

(setq custom-file (expand-file-name "custom.el" lolo-personal-dir))

(provide 'lolo-const)
;;; lolo-const.el ends here
