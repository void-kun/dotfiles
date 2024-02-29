
;;; lolo-dict.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(eval-when-compile
  (require 'lolo-const))

;; A multi dictionaries interface
(use-package fanyi
  :bind (("C-c d f" . fanyi-dwim)
         ("C-c d d" . fanyi-dwim2)
         ("C-c d h" . fanyi-from-history))
  :custom (fanyi-providers '(fanyi-haici-provider fanyi-longman-provider))

  (use-package go-translate
    :bind (("C-c d g" . gts-do-translate))
    :init (setq gts-translate-list '(("en" "zh") ("zh" "en")))))

(provide 'lolo-dict)
;;; lolo-dict.el ends here
