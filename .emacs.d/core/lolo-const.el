;;; lolo-const.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(defconst sys/linuxp (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/linux-x-p (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst lolo-custom-example-file
  (expand-file-name "custom-example.el" user-emacs-directory)
  "Custom example file of Lolo Emacs.")

(defconst lolo-custom-post-file
  (expand-file-name "custom-post.el" user-emacs-directory)
  "Custom file after startup.

Put private configurations to override defaults here.")

(provide 'lolo-const)
;;; lolo-const.el ends here
