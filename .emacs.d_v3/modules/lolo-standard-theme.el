;;; lolo-standard-theme.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package standard-themes
  :ensure t
  :demand t
  :config
  (setq standard-themes-bold-constructs t
        standard-themes-italic-constructs t
        standard-themes-mixed-fonts t
        standard-themes-variable-pitch-ui t
        standard-themes-mode-line-accented nil

        ;; Accepts a symbol value
        standard-themes-fringes 'subtle

        ;; The following accept lists of properties
        standard-themes-links nil
        standard-themes-region nil
        standard-themes-prompts nil

        ;; more complex alist to set weight, height, and optional
        ;; `variable-pitch' per heading level (t is for any level not
        ;; specified)
        standard-themes-headings
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch light 1.7))
          (3 . (variable-pitch semilight 1.6))
          (4 . (variable-pitch semilight 1.5))
          (5 . (variable-pitch 1.4))
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (agenda-date . (1.3))
          (agenda-structure . (variable-pitch light 1.8))
          (t . (variable-pitch 1.1))))

  ;; Load a theme that is consistent with my session's theme.  Those
  ;; functions are defined in my init.el.
  (if lolo-theme-dark
      (standard-themes-load-dark)
    (standard-themes-load-light)))

(provide 'lolo-standard-theme)
;;; lolo-standard-theme.el ends here
