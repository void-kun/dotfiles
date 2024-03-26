;;; lolo-emacs-lisp.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package emacs-lisp-mode
  :ensure nil
  :general
  (general-define-key
   :prefix ","
   :states 'motion
   :keymaps 'emacs-lisp-mode-map
   "" nil
   "e" '(nil :which-key "eval")
   "es" '(eval-last-sexp :which-key "eval-sexp")
   "er" '(eval-region :which-key "eval-region")
   "eb" '(eval-buffer :which-key "eval-buffer")

   "g" '(counsel-imenu :which-key "imenu")
   "c" '(check-parens :which-key "check parens")
   "I" '(indent-region :which-key "indent-region")

   "b" '(nil :which-key "org src")
   "bc" 'org-edit-src-abort
   "bb" 'org-edit-src-exit
   )
  )

(provide 'lolo-emacs-lisp)
;;; lolo-emacs-lisp.el ends here
