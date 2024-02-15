;;; typescript-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from typescript-mode.el

(put 'typescript-indent-level 'safe-local-variable #'integerp)
(autoload 'typescript-mode "typescript-mode" "\
Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

(fn)" t)
(eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}")))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(register-definition-prefixes "typescript-mode" '("typescript-"))


;;; Generated autoloads from typescript-mode-test-utilities.el

(register-definition-prefixes "typescript-mode-test-utilities" '("font-lock-test" "get-face-at" "test-with-"))

;;; End of scraped data

(provide 'typescript-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; typescript-mode-autoloads.el ends here
