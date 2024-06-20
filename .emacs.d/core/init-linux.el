;;; init-linux.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; On Linux Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(lolo/require-packages '(exec-path-from-shell))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(provide 'init-linux)
;;; init-linux.el ends here
