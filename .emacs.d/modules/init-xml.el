;;; init-xml.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'nxml-mode)

(push '("<\\?xml" . nxml-mode) magic-mode-alist)
;; pom files should be treated as xml files
(add-to-list 'auto-mode-alist '("\\.pom\\'" . nxml-mode))
(setq nxml-child-indent 4)
(setq nxml-attribute-indent 4)
(setq nxml-auto-insert-xml-declaration-flag nil)
(setq nxml-bind-meta-tab-to-complete-flag t)
(setq nxml-slash-auto-complete-flag t)

(provide 'init-xml)
;;; init-xml.el ends here
