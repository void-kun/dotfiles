;;; lolo-terraform.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:


(when (maybe-require-package 'terraform-mode)
  ;; TODO: find/write a replacement for company-terraform
  (with-eval-after-load 'terraform-mode
    ;; I find formatters based on "reformatter" to be more reliable
    ;; so I redefine `terraform-format-on-save-mode' here.
    (when (maybe-require-package 'reformatter)
      (reformatter-define terraform-format
        :program "terraform" :args '("fmt" "-")))))

        
(provide 'lolo-terraform)
;;; lolo-terraform.el ends here
