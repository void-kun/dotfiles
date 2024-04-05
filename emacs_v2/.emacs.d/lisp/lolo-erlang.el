;;; lolo-erlang.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:


(when (maybe-require-package 'erlang)
  (require 'erlang-start))

(provide 'lolo-erlang)
;;; lolo-erlang.el ends here
