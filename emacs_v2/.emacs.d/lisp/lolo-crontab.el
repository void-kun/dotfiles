;;; lolo-crontab.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:


(require-package 'crontab-mode)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")

(provide 'lolo-crontab)
;;; lolo-crontab.el ends here
