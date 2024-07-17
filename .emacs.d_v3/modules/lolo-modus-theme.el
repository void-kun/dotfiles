;;; lolo-modus-theme.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 modus-themes
 :ensure t
 :demand t
 :config
 (setq
  modus-themes-custom-auto-reload nil
  modus-themes-to-toggle '(modus-operandi modus-vivendi)
  ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
  ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
  ;; modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
  modus-themes-mixed-fonts t
  modus-themes-variable-pitch-ui t
  modus-themes-italic-constructs t
  modus-themes-bold-constructs nil
  modus-themes-completions '((t . (extrabold)))
  modus-themes-prompts '(extrabold)
  modus-themes-headings
  '((agenda-structure . (variable-pitch light 2.2))
    (agenda-date . (variable-pitch regular 1.3))
    (t . (regular 1.15))))

 (setq modus-themes-common-palette-overrides nil)

 (if lolo-theme-dark
     (modus-themes-load-theme (cadr modus-themes-to-toggle))
   (modus-themes-load-theme (car modus-themes-to-toggle))))

(provide 'lolo-modus-theme)
;;; lolo-modus-theme.el ends here
