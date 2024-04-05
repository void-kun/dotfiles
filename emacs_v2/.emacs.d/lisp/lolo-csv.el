;;; lolo-csv.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:


(when (maybe-require-package 'csv-mode)
  (add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

  (setq csv-separators '("," ";" "|" " ")))

(provide 'lolo-csv)
;;; lolo-csv.el ends here
