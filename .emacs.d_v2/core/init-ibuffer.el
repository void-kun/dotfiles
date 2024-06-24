;;; init-ibuffer.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 ibuffer
 :ensure nil
 :bind ("C-x C-b" . ibuffer)
 :init
 (use-package
  ibuffer-vc
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :custom (ibuffer-vc-skip-if-remote 'nil))
 :custom
 (ibuffer-formats
  '((mark
     modified
     read-only
     locked
     " "
     (name 35 35 :left :elide)
     " "
     (size 9 -1 :right)
     " "
     (mode 16 16 :left :elide)
     " "
     filename-and-process)
    (mark " " (name 16 -1) " " filename))))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
