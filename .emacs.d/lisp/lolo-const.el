
;;; lolo-const.el --- xxx.	-*- lexical-binding: t -*-

;; Author: hoangzrik
;; URL: https://github.com/void-kun/dotfiles

;;; Code:

(defconst lolo-custom-file
    (expand-file-name "custom.el" user-emacs-directory)
    "Custom file of Emacs.")

(defconst lolo-post-file
    (expand-file-name "post.el" user-emacs-directory)
    "Custom file after startup.")

(defconst lolo-post-org-file
  (expand-file-name "post.org" user-emacs-directory)
  "Custom org file after startup.")
  
(defconst sys/linuxp
    (eq system-type 'gnu/linux)
    "Are we running on a GNU/Linux system?")

(defconst sys/linux-x-p
    (and (display-graphic-p) sys/linuxp)
    "Are we running under X on a GNU/Linux system?")

(provide 'lolo-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lolo-const.el ends here
