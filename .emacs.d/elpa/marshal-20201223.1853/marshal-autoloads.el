;;; marshal-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from marshal.el

(autoload 'marshal "marshal" "\


(fn OBJ TYPE)")
(autoload 'unmarshal "marshal" "\


(fn OBJ BLOB TYPE)")
(autoload 'marshal-defclass "marshal" "\


(fn NAME SUPERCLASS SLOTS &rest OPTIONS-AND-DOC)" nil t)
(function-put 'marshal-defclass 'lisp-indent-function 2)
(register-definition-prefixes "marshal" '("marshal-" "unmarshal-internal"))

;;; End of scraped data

(provide 'marshal-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; marshal-autoloads.el ends here
