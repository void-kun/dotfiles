;;; elisp-slime-nav-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from elisp-slime-nav.el

(autoload 'elisp-slime-nav-mode "elisp-slime-nav" "\
Enable Slime-style navigation of elisp symbols using M-. and M-,

This is a minor mode.  If called interactively, toggle the
`Elisp-Slime-Nav mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `elisp-slime-nav-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(define-obsolete-function-alias 'turn-on-elisp-slime-nav-mode 'elisp-slime-nav-mode "2020-01-30")
(autoload 'elisp-slime-nav-find-elisp-thing-at-point "elisp-slime-nav" "\
Find the elisp thing at point, be it a function, variable, library or face.

With a prefix arg, or if there is no thing at point, prompt for
the symbol to jump to.

Argument SYM-NAME is the thing to find.

(fn SYM-NAME)" t)
(autoload 'elisp-slime-nav-describe-elisp-thing-at-point "elisp-slime-nav" "\
Display the full documentation of the elisp thing at point.

The named subject may be a function, variable, library or face.

With a prefix arg, or if there is not \"thing\" at point, prompt
for the symbol to jump to.

Argument SYM-NAME is the thing to find.

(fn SYM-NAME)" t)
(register-definition-prefixes "elisp-slime-nav" '("elisp-slime-nav-"))

;;; End of scraped data

(provide 'elisp-slime-nav-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; elisp-slime-nav-autoloads.el ends here
