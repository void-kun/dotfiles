;;; lolo-ef-theme.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 ef-themes
 :ensure t
 :demand t
 :config
 (setq
  ef-themes-variable-pitch-ui t
  ef-themes-mixed-fonts t
  ef-themes-headings ; read the manual's entry of the doc string
  '((0 . (variable-pitch light 1.9))
    (1 . (variable-pitch light 1.8))
    (2 . (variable-pitch regular 1.7))
    (3 . (variable-pitch regular 1.6))
    (4 . (variable-pitch regular 1.5))
    (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
    (6 . (variable-pitch 1.3))
    (7 . (variable-pitch 1.2))
    (agenda-date . (semilight 1.5))
    (agenda-structure . (variable-pitch light 1.9))
    (t . (variable-pitch 1.1))))

 (if lolo-theme-dark
     (ef-themes-load-random 'dark)
   (ef-themes-load-random 'light)))

(provide 'lolo-ef-theme)
;;; lolo-ef-theme.el ends here
